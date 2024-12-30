-- migrate:up
CREATE TABLE "identity" (
	"id"	INTEGER,
	"account"	TEXT UNIQUE,
	"localpart"	TEXT UNIQUE,
	PRIMARY KEY("id" AUTOINCREMENT)
);

-- migrate:down
DROP TABLE "identity";
