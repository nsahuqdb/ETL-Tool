# TODO — IFRS9 ETL parking lot

Items deferred from the current build, kept here so they don't get
lost. Items at the top are the most likely "next thing".

## User management & roles

The current app uses `Sys.info()[["user"]]` as identity — fine for a
single-user dev/test environment, not enough for production. Things
that need this to be solved first:

- **Login / accounts** — replace `Sys.info()[["user"]]` with a real
  identity. Probably integrates with QDB's existing SSO (Active
  Directory / Azure AD).
- **Roles per user** — at least: maker, checker, approver, admin.
  Likely also "viewer" (read-only).
- **Role-based UI** — hide Approve buttons from users who can't act
  on the current stage. Hide snapshot edit from non-admins. Etc.
- **Approval separation of duties enforcement** — currently lives
  behind a config flag (`approval.enforce_separation_of_duties`).
  When true, refuses approval if the same user ran AND is approving.
  Currently checks identity from `Sys.info()`; needs to switch to
  the logged-in user once login lands.

## Approval workflow extensions

- **Email notifications** — when a run advances to `pending_checker`,
  email the assigned checker. When approved/rejected, email the
  maker.
- **Reassignment / delegation** — if the assigned approver is on
  leave, allow an admin to reassign the pending review.
- **Three-stage approval** for high-stakes runs — checker AND a
  separate final-approver. Currently the schema supports this (the
  state machine could be extended) but only the 2-stage flow is
  wired in the UI.
- **Approval rules per snapshot or run type** — e.g. "production
  runs need checker + final approver; ad-hoc runs only need
  checker."
- **Snapshot promotion two-step approval** — currently single-stage.

## Other deferred work

- **Async runs** — phase 2 takes ~20-30s during which the UI is
  locked. `promises` + `future` would unblock. Watch for race
  conditions on the audit log tail.
- **Suppression approvals** — adding a validator suppression today
  goes through whoever the user is, no review. In production should
  be a separate approval workflow.
- **Input data approval** — does the bank's data extract from the
  source system look reasonable? Currently implicit. May want a
  human review step before allowing pre-run check.
- **Multi-tenant / per-team approval rules** — if QDB ever splits
  this across departments.
- **Mass-backfill old runs with `config_used/`** — older runs predate
  the runtime config-freeze. Could write a one-shot script.
- **Two-step snapshot promotion** — same maker/checker pattern for
  config edits.

## Analytical items (no app changes, just memos)

- StPD value drift (max_abs_diff = 0.109) — the V7 `Calc!BQ245+`
  typo we deliberately don't replicate. Needs a memo.
- Investments-side reconciliation drift — 73/73 row count match,
  values drift in some columns.
- Lifetime PD methodology — cumsum vs survival.
