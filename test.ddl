create_table apps AP (
    name text not null,
    description text not null,
    website_url text null,
    client_id text not null,
    client_secret text not null,
    organisation_id text not null,
    created_at timestamp not null,
    updated_at timestamp not null,
    must_use_payment_pages boolean not null,
    creditor_id text not null,
    refunds_enabled boolean not null,
    logo_id text null,
    post_onboarding_url text null,
    category text null,
    legacy_app_id text null,
    can_create_bulk_changes boolean null,
    excluded_from_payer_experiments boolean null
)
add_column apps created_at timestamp
-- add_index apps (created_at) apps_created_at_index
drop_index apps_created_at_index
