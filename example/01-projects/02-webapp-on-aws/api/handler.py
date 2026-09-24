import base64
import json
import os
import sys

# Deployment dependencies (see requirements.txt) are vendored into this
# directory by `lask run build_lambda_deps` rather than relying on a Lambda
# Layer. Add it to sys.path so it resolves both when Lambda unzips this
# folder to /var/task and when running locally/in tests.
_VENDOR_DIR = os.path.join(os.path.dirname(__file__), "vendor")
if os.path.isdir(_VENDOR_DIR) and _VENDOR_DIR not in sys.path:
    sys.path.insert(0, _VENDOR_DIR)

import pg8000.native  # noqa: E402 (import must follow the sys.path setup above)


def _get_connection():
    """Opens a new Postgres connection, or None if DB env vars aren't set."""
    host = os.environ.get("DB_HOST")
    if not host:
        return None

    return pg8000.native.Connection(
        user=os.environ["DB_USER"],
        password=os.environ["DB_PASSWORD"],
        host=host,
        port=int(os.environ.get("DB_PORT", "5432")),
        database=os.environ.get("DB_NAME", "postgres"),
    )


def _check_database():
    """Connects to Postgres and returns a short status string.

    Any failure (missing env vars, connection error, etc.) is caught so a
    database issue never turns this demo endpoint into a 500.
    """
    conn = None
    try:
        conn = _get_connection()
        if conn is None:
            return "not configured"
        conn.run("SELECT 1")
        return "connected"
    except Exception as exc:  # noqa: BLE001 - surface any failure as a status string
        return f"error: {exc}"
    finally:
        if conn is not None:
            conn.close()


def _ensure_orders_table(conn):
    # Runs on every /orders request rather than as a separate migration
    # step, to keep this example's deploy pipeline simple. CREATE TABLE IF
    # NOT EXISTS is idempotent, so this is safe (if wasteful at scale).
    conn.run(
        """
        CREATE TABLE IF NOT EXISTS orders (
            id SERIAL PRIMARY KEY,
            item TEXT NOT NULL,
            quantity INTEGER NOT NULL,
            created_at TIMESTAMPTZ NOT NULL DEFAULT now()
        )
        """
    )


def _row_to_order(row):
    return {
        "id": row[0],
        "item": row[1],
        "quantity": row[2],
        "created_at": row[3].isoformat(),
    }


def _json_response(status_code, body):
    return {
        "statusCode": status_code,
        "headers": {"Content-Type": "application/json"},
        "body": json.dumps(body),
    }


def _list_orders():
    conn = _get_connection()
    if conn is None:
        return _json_response(503, {"error": "database not configured"})

    try:
        _ensure_orders_table(conn)
        rows = conn.run("SELECT id, item, quantity, created_at FROM orders ORDER BY id")
        return _json_response(200, {"orders": [_row_to_order(row) for row in rows]})
    except Exception as exc:  # noqa: BLE001 - surface any failure as a 500
        return _json_response(500, {"error": str(exc)})
    finally:
        conn.close()


def _create_order(raw_body):
    try:
        payload = json.loads(raw_body or "{}")
    except json.JSONDecodeError:
        return _json_response(400, {"error": "invalid JSON body"})

    item = payload.get("item")
    quantity = payload.get("quantity")
    if not isinstance(item, str) or not item.strip():
        return _json_response(400, {"error": "item is required"})
    if not isinstance(quantity, int) or isinstance(quantity, bool) or quantity <= 0:
        return _json_response(400, {"error": "quantity must be a positive integer"})

    conn = _get_connection()
    if conn is None:
        return _json_response(503, {"error": "database not configured"})

    try:
        _ensure_orders_table(conn)
        row = conn.run(
            "INSERT INTO orders (item, quantity) VALUES (:item, :quantity) "
            "RETURNING id, item, quantity, created_at",
            item=item.strip(),
            quantity=quantity,
        )[0]
        return _json_response(201, {"order": _row_to_order(row)})
    except Exception as exc:  # noqa: BLE001 - surface any failure as a 500
        return _json_response(500, {"error": str(exc)})
    finally:
        conn.close()


def _delete_order(order_id):
    conn = _get_connection()
    if conn is None:
        return _json_response(503, {"error": "database not configured"})

    try:
        _ensure_orders_table(conn)
        deleted = conn.run("DELETE FROM orders WHERE id = :id RETURNING id", id=order_id)
        if not deleted:
            return _json_response(404, {"error": f"order {order_id} not found"})
        # 204 carries no body, so no Content-Type either.
        return {"statusCode": 204, "headers": {}, "body": ""}
    except Exception as exc:  # noqa: BLE001 - surface any failure as a 500
        return _json_response(500, {"error": str(exc)})
    finally:
        conn.close()


def _extract_method_and_path(event):
    """Supports both the API Gateway REST (v1) proxy format and the Lambda
    Function URL / HTTP API (v2, payload format 2.0) event format."""
    if "httpMethod" in event:
        return event.get("httpMethod", "GET"), event.get("path", "/")

    http = event.get("requestContext", {}).get("http", {})
    return http.get("method", "GET"), event.get("rawPath", "/")


def _extract_body(event):
    body = event.get("body")
    if body and event.get("isBase64Encoded"):
        body = base64.b64decode(body).decode("utf-8")
    return body


def lambda_handler(event, context):
    method, path = _extract_method_and_path(event)
    path = path.rstrip("/") or "/"

    if path == "/orders" and method == "GET":
        return _list_orders()
    if path == "/orders" and method == "POST":
        return _create_order(_extract_body(event))

    if path.startswith("/orders/"):
        raw_id = path[len("/orders/"):]
        if method != "DELETE":
            return _json_response(405, {"error": f"{method} is not allowed on {path}"})
        try:
            order_id = int(raw_id)
        except ValueError:
            return _json_response(400, {"error": f"invalid order id: '{raw_id}'"})
        return _delete_order(order_id)

    # Default: demo/health endpoint.
    response_body = {
        "message": "Hello from Lask API",
        "method": method,
        "path": path,
        "database": _check_database(),
    }
    return _json_response(200, response_body)
