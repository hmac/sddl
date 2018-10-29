# sddl

A DSL for safe database migrations.

# Try it out

Head to the Releases page on GitHub and download the current version.

## Examples

```
echo "[{tag: add_column, table: foo, column: bar, type: timestamp}]" | sddl --format yaml
echo "[{tag: add_column, table: foo, column: bar, type: timestamp}]" | sddl -r --format sql
```
