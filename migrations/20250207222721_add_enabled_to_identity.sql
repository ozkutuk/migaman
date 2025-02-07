-- migrate:up
ALTER TABLE "identity" ADD COLUMN "enabled" INTEGER NOT NULL DEFAULT 1;

-- migrate:down
ALTER TABLE "identity" DROP COLUMN "enabled";
