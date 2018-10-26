create_table apps AP (
    name text not null,
    description text not null,
    created_at timestamp not null
)
add_column apps created_at timestamp
drop_index apps_created_at_index
