# Releasing

This project uses [cargo-release](https://github.com/crate-ci/cargo-release) to automate releases.

## First-time setup

```bash
cargo install cargo-release
cargo login
```

The `cargo login` command requires an API token from https://crates.io/settings/tokens.

## Releasing

Dry-run (no changes made):

```bash
cargo release patch   # or minor, major
```

Execute the release:

```bash
cargo release patch --execute
```

This will:

1. Bump the version in `Cargo.toml`
2. Commit the version bump
3. Create a git tag (`vX.Y.Z`)
4. Publish to crates.io
5. Push the commit and tag to GitHub

After the release, create a GitHub Release from the tag:

```bash
gh release create vX.Y.Z --title "vX.Y.Z" --generate-notes
```
