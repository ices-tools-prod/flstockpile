-- flstock_db_schema.sql
--
-- One row per versioned FLStock JSON document.
-- Metadata columns are extracted for querying; `json` holds the full
-- document verbatim, so a row can always be converted back via
-- json_to_flstock().

CREATE TABLE IF NOT EXISTS flstock_versions (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  stock_id    TEXT    NOT NULL,
  version     TEXT    NOT NULL,
  name        TEXT,
  description TEXT,
  created_by  TEXT,
  created_at  TEXT    NOT NULL,
  source      TEXT,
  quant       TEXT,
  minyear     INTEGER,
  maxyear     INTEGER,
  minfbar     REAL,
  maxfbar     REAL,
  plusgroup   REAL,
  json        TEXT    NOT NULL,
  UNIQUE(stock_id, version)
);
