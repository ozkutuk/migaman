-- migrate:up
ALTER TABLE "identity" ADD COLUMN "domain" TEXT;
ALTER TABLE "identity" ADD COLUMN "target" TEXT;

-- migrate:down
ALTER TABLE "identity" DROP COLUMN "domain";
ALTER TABLE "identity" DROP COLUMN "target";
