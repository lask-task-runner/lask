# Injects the real example main.lask into the PV page.
import json, pathlib, sys
here = pathlib.Path(__file__).parent
src = pathlib.Path(sys.argv[1]).read_text()
page = (here / "pv.src.html").read_text().replace("__MAIN_LASK__", json.dumps(src))
(here / "pv.html").write_text(page)
