let SqlType = ./sql.dhall
in < AddColumn : { table : Text, column: Text, sqlType: SqlType }
   | CreateTable : { name: Text, prefix: Text, definition: List ./column.dhall }
   | MakeColumnNotNull : { table: Text, column: Text }
   | MakeColumnNull : { table: Text, column: Text }
   | CreateIndex : { table: Text, columns: List Text, name: Text, partial: Optional Text }
   | DropIndex : { name: Text }
   | DropTable : { name: Text }
   | AddForeignKey : { sourceTable: Text, sourceColumn: Text, targetTable: Text, targetColumn: Text, name: Text }
   >
