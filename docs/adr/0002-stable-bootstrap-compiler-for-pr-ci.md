# Stable bootstrap compiler for PR CI

PR CI uses the latest stable released **Bootstrap compiler** by default: the release tag lives in **casa-release.env**, and CI builds the branch compiler from that stable release before checking self-compilation and **Fixed point** behavior. Compiler-language changes should be staged so they remain bootstrappable from the stable release; a **Temporary compiler release** is allowed only as part of **Staged bootstrap repair**, guarded by the `bootstrap-override` pull request label and followed by cleanup back to a stable release tag.

**Considered Options**

- Stable bootstrap by default: stricter, but keeps compiler provenance simple and makes PR failures meaningful.
- Unreleased compiler artifacts as a normal CI input: more convenient for breaking language changes, but creates a hidden compiler lineage and makes it easier to merge branches that cannot be built from the stable compiler.
- Manual temporary releases without enforcement: works in emergencies, but is easy to forget and can permanently pin CI to a prerelease.
