import json
import os
import unittest
from datetime import datetime, timezone
from unittest.mock import MagicMock, patch

from handler import lambda_handler

DB_ENV = {"DB_HOST": "db.example.com", "DB_USER": "u", "DB_PASSWORD": "p", "DB_NAME": "d"}


def v2_event(method, path, body=None):
    """A Lambda Function URL (payload format 2.0) style event."""
    event = {
        "requestContext": {"http": {"method": method}},
        "rawPath": path,
    }
    if body is not None:
        event["body"] = body
    return event


class HealthEndpointTest(unittest.TestCase):
    def test_reports_request_information_from_v2_event(self):
        response = lambda_handler(v2_event("GET", "/"), None)

        self.assertEqual(response["statusCode"], 200)
        self.assertEqual(response["headers"]["Content-Type"], "application/json")
        self.assertEqual(
            json.loads(response["body"]),
            {
                "message": "Hello from Lask API",
                "method": "GET",
                "path": "/",
                "database": "not configured",
            },
        )

    def test_reports_request_information_from_v1_event(self):
        # Backward compatibility with the API Gateway REST (v1) proxy format.
        response = lambda_handler({"httpMethod": "PUT", "path": "/settings"}, None)

        body = json.loads(response["body"])
        self.assertEqual(body["method"], "PUT")
        self.assertEqual(body["path"], "/settings")

    def test_uses_defaults_for_empty_event(self):
        response = lambda_handler({}, None)

        self.assertEqual(
            json.loads(response["body"]),
            {
                "message": "Hello from Lask API",
                "method": "GET",
                "path": "/",
                "database": "not configured",
            },
        )

    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_reports_connected_when_database_is_reachable(self, mock_connection_cls):
        mock_conn = MagicMock()
        mock_connection_cls.return_value = mock_conn

        response = lambda_handler(v2_event("GET", "/"), None)

        body = json.loads(response["body"])
        self.assertEqual(body["database"], "connected")
        mock_conn.run.assert_called_once_with("SELECT 1")
        mock_conn.close.assert_called_once()

    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection", side_effect=RuntimeError("connection refused"))
    def test_reports_error_when_database_is_unreachable(self, mock_connection_cls):
        response = lambda_handler(v2_event("GET", "/"), None)

        body = json.loads(response["body"])
        self.assertEqual(body["database"], "error: connection refused")


class ListOrdersTest(unittest.TestCase):
    def test_returns_503_when_database_is_not_configured(self):
        response = lambda_handler(v2_event("GET", "/orders"), None)

        self.assertEqual(response["statusCode"], 503)

    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_returns_orders_ordered_by_id(self, mock_connection_cls):
        mock_conn = MagicMock()
        created_at = datetime(2024, 1, 1, tzinfo=timezone.utc)
        mock_conn.run.side_effect = [
            None,  # CREATE TABLE IF NOT EXISTS
            [[1, "Widget", 3, created_at]],  # SELECT
        ]
        mock_connection_cls.return_value = mock_conn

        response = lambda_handler(v2_event("GET", "/orders"), None)

        self.assertEqual(response["statusCode"], 200)
        self.assertEqual(
            json.loads(response["body"]),
            {"orders": [{"id": 1, "item": "Widget", "quantity": 3, "created_at": created_at.isoformat()}]},
        )
        mock_conn.close.assert_called_once()


class CreateOrderTest(unittest.TestCase):
    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_creates_an_order(self, mock_connection_cls):
        mock_conn = MagicMock()
        created_at = datetime(2024, 1, 1, tzinfo=timezone.utc)
        mock_conn.run.side_effect = [
            None,  # CREATE TABLE IF NOT EXISTS
            [[1, "Widget", 3, created_at]],  # INSERT ... RETURNING
        ]
        mock_connection_cls.return_value = mock_conn

        body = json.dumps({"item": "Widget", "quantity": 3})
        response = lambda_handler(v2_event("POST", "/orders", body=body), None)

        self.assertEqual(response["statusCode"], 201)
        self.assertEqual(
            json.loads(response["body"]),
            {"order": {"id": 1, "item": "Widget", "quantity": 3, "created_at": created_at.isoformat()}},
        )
        insert_call = mock_conn.run.call_args_list[1]
        self.assertEqual(insert_call.kwargs, {"item": "Widget", "quantity": 3})

    def test_returns_400_for_missing_item(self):
        body = json.dumps({"quantity": 3})
        response = lambda_handler(v2_event("POST", "/orders", body=body), None)

        self.assertEqual(response["statusCode"], 400)
        self.assertIn("item", json.loads(response["body"])["error"])

    def test_returns_400_for_non_positive_quantity(self):
        body = json.dumps({"item": "Widget", "quantity": 0})
        response = lambda_handler(v2_event("POST", "/orders", body=body), None)

        self.assertEqual(response["statusCode"], 400)
        self.assertIn("quantity", json.loads(response["body"])["error"])

    def test_returns_400_for_invalid_json(self):
        response = lambda_handler(v2_event("POST", "/orders", body="not json"), None)

        self.assertEqual(response["statusCode"], 400)

    def test_returns_503_when_database_is_not_configured(self):
        body = json.dumps({"item": "Widget", "quantity": 3})
        response = lambda_handler(v2_event("POST", "/orders", body=body), None)

        self.assertEqual(response["statusCode"], 503)


class DeleteOrderTest(unittest.TestCase):
    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_deletes_an_existing_order(self, mock_connection_cls):
        mock_conn = MagicMock()
        mock_conn.run.side_effect = [
            None,  # CREATE TABLE IF NOT EXISTS
            [[7]],  # DELETE ... RETURNING id
        ]
        mock_connection_cls.return_value = mock_conn

        response = lambda_handler(v2_event("DELETE", "/orders/7"), None)

        self.assertEqual(response["statusCode"], 204)
        self.assertEqual(response["body"], "")
        delete_call = mock_conn.run.call_args_list[1]
        self.assertEqual(delete_call.kwargs, {"id": 7})
        mock_conn.close.assert_called_once()

    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_returns_404_for_a_missing_order(self, mock_connection_cls):
        mock_conn = MagicMock()
        mock_conn.run.side_effect = [None, []]  # DELETE matched no row
        mock_connection_cls.return_value = mock_conn

        response = lambda_handler(v2_event("DELETE", "/orders/99"), None)

        self.assertEqual(response["statusCode"], 404)
        self.assertIn("99", json.loads(response["body"])["error"])

    def test_returns_400_for_a_non_numeric_id(self):
        response = lambda_handler(v2_event("DELETE", "/orders/abc"), None)

        self.assertEqual(response["statusCode"], 400)
        self.assertIn("abc", json.loads(response["body"])["error"])

    def test_returns_405_for_other_methods_on_an_order(self):
        response = lambda_handler(v2_event("GET", "/orders/1"), None)

        self.assertEqual(response["statusCode"], 405)

    def test_returns_503_when_database_is_not_configured(self):
        response = lambda_handler(v2_event("DELETE", "/orders/1"), None)

        self.assertEqual(response["statusCode"], 503)

    @patch.dict(os.environ, DB_ENV)
    @patch("handler.pg8000.native.Connection")
    def test_ignores_a_trailing_slash(self, mock_connection_cls):
        mock_conn = MagicMock()
        mock_conn.run.side_effect = [None, [[7]]]
        mock_connection_cls.return_value = mock_conn

        response = lambda_handler(v2_event("DELETE", "/orders/7/"), None)

        self.assertEqual(response["statusCode"], 204)


if __name__ == "__main__":
    unittest.main()
