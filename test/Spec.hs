{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
import           Control.Monad   (forM_)
import           Test.Hspec

import           Core
import           Data.ByteString (ByteString)
import           Data.Yaml       (decodeThrow)
import           Numeric.Natural (Natural)
import           Print           (toSql)
import           Validate        (buildMigration)

main :: IO ()
main = hspec $
  describe "correct migrations" $
    it "generate the correct YAML" $
      forM_ testTable $ \(yaml, sql, lockT, statementT, tx) -> do
        ss <- decodeThrow yaml
        let Right Migration { statements, lockTimeout, statementTimeout, transaction } = buildMigration ss
        toSql (head statements) `shouldBe` sql
        lockTimeout `shouldBe` lockT
        statementTimeout `shouldBe` statementT
        transaction `shouldBe` tx

testTable :: [(ByteString, String, Natural, Natural, Bool)]
testTable =
  [
    ( "- { tag: drop_column, table: foo, column: bar }"
    , "ALTER TABLE foo DROP COLUMN bar"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: add_column, table: foo, column: bar, type: text }"
    , "ALTER TABLE foo ADD COLUMN bar text"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: drop_table, table: foo }"
    , "DROP TABLE foo"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: make_column_null, table: foo, column: bar }"
    , "ALTER TABLE ONLY foo ALTER COLUMN bar SET NULL"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: make_column_not_null, table: foo, column: bar }"
    , "ALTER TABLE ONLY foo ALTER COLUMN bar SET NOT NULL"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: change_column_default, table: foo, column: bar, from: a, to: b }"
    , "ALTER TABLE ONLY foo ALTER COLUMN bar SET DEFAULT 'b'"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: drop_column_default, table: foo, column: bar, from: a }"
    , "ALTER TABLE ONLY foo ALTER COLUMN bar DROP DEFAULT"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: create_index, table: foo, columns: [bar, baz], name: my_idx }"
    , "CREATE INDEX CONCURRENTLY my_idx ON foo USING btree (bar , baz)"
    , 900000
    , 900000
    , False
    )
  , ( "- { tag: drop_index, name: my_idx }"
    , "DROP INDEX CONCURRENTLY my_idx"
    , 900000
    , 900000
    , False
    )
  , ( "- { tag: add_foreign_key, source_table: foo, source_column: bar, target_table: boo, target_column: far, name: my_fk }"
    , "ALTER TABLE ONLY foo ADD CONSTRAINT my_fk FOREIGN KEY (bar) REFERENCES boo(far) NOT VALID"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: validate_foreign_key, table: foo, name: my_fk }"
    , "ALTER TABLE ONLY foo VALIDATE CONSTRAINT my_fk"
    , 900000
    , 900000
    , False
    )
  , ( "- { tag: drop_constraint, table: foo, name: my_fk }"
    , "ALTER TABLE ONLY foo DROP CONSTRAINT my_fk"
    , 750
    , 1500
    , True
    )
  , ( "- { tag: create_table, table: foo, prefix: FO, definition: [{column: bar, type: text, null: false}, {column: baz, type: boolean, null: true}] }"
    , "CREATE SEQUENCE public.foo_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;\nCREATE TABLE foo (\nid character varying(32) DEFAULT public.gen_gc_id('FO'::text, 'foo_id_seq'::text) NOT NULL,\nbar text NOT NULL,\nbaz boolean NULL,\n)"
    , 750
    , 1500
    , True
    )
  ]
