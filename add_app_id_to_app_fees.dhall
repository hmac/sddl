let Migrate = constructors ./migration.dhall
in let Sql = constructors ./sql.dhall
in [
  Migrate.AddColumn { table = "app_fees", column = "app_id", sqlType = Sql.SText {=} }
  ]
