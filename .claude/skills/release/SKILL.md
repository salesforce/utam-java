---
name: release
description: >-
  Release the UTAM Java framework (utam-core and utam-compiler) to Maven Central.
  Use when the user asks to cut, publish, or ship a UTAM Java release — a standard
  release from a release branch, or a hotfix release from main. Covers prerequisites
  (Sonatype account, settings.xml, GPG signing key), the standard and hotfix flows,
  release confirmation, and downstream steps (updating the version in core and the
  utam-java-framework repo).
---

# UTAM Java Framework Release Process

This skill guides releasing the UTAM Java artifacts — `com.salesforce.utam:utam-core`
and `com.salesforce.utam:utam-compiler` — to the Maven Central Repository.

> Modules in this repo: `utam-core`, `utam-compiler` (see `<modules>` in `pom.xml`).
> The root `pom.xml` version currently ends in `-SNAPSHOT`; that snapshot value minus
> `-SNAPSHOT` is the release version, and the next dev iteration bumps the patch and
> re-adds `-SNAPSHOT`.

## ⚠️ Hard rules — read before doing anything

- **Never, ever merge a pull request into `main`.** Only ever merge **directly from
  `dev` to `main`** (or from a release/hotfix branch into `main`).
- Merges between `dev` and `main` (either direction) should **almost always be
  fast-forward**. If a merge is *not* fast-forward, something is wrong — fixing it
  needs a force push and can trigger unwanted CI builds / Nexus publishes. Stop and
  investigate rather than pushing.
- **Do not start the release unless you can finish it end-to-end.** You need the
  Sonatype account, publish permissions, `settings.xml` server entry, and the GPG
  private key + passphrase. A halted release can leave the repo in a bad state. This
  is deliberately not a one-command script for that reason.
- This process assumes you work directly against the main repo (not a fork) and your
  `origin` remote points at it. For the primary Maven-plugin flow the repo **must be
  cloned over SSH, not HTTPS**.

## Prerequisites (one-time setup)

Confirm all of these before starting. If any are missing, stop and get them sorted first.

1. **Buildable environment** — working Git, a suitable JDK, and Maven; you can already
   build this repo locally.
2. **Sonatype Maven Central account** with permission to publish to the Salesforce UTAM
   coordinates. Sign up at https://issues.sonatype.org/secure/Signup!default.jspa;
   publish rights require a JIRA ticket approved by an existing admin — ask a teammate.
3. **`~/.m2/settings.xml` server entry** for the Central staging server:
   - Log in to https://central.sonatype.com with your account.
   - Top-right → your name → **View Account** → **Generate User Token**.
   - Copy the `<server>` XML snippet and paste it as a child of `<servers>` in
     `~/.m2/settings.xml`.
   - **Change the `<id>` to `central.repository.publishing`.** Leave the hashed
     username/password as generated.
4. **GPG key for signing** (GPG `2.1.0`+ required):
   - Install: `brew install gpg` (macOS) or `sudo apt-get install gpg` (Ubuntu).
   - Public key: `gpg --keyserver keyserver.ubuntu.com --recv-keys 68F4347A067873800184ECC7B6C745380D643F2D`
   - Private key + passphrase live in the **QI Foundation vault in 1Password** (or get
     them from a teammate who has released). Import with
     `gpg --import <private-key-file-path>`. **Never** commit the private key or
     passphrase, or post them to Slack/docs.

## Choosing a flow

- **Standard release** — cutting a normal release from a `<release>` branch that was
  branched off `dev`. Use the [primary flow](#standard-release--primary-maven-release-plugin)
  if cloned over SSH; otherwise use the [alternative flow](#standard-release--alternative-manual-version-bump).
- **Hotfix release** — an urgent fix cut from `main`. See [Hotfix release](#hotfix-release).

Define these terms before running commands (ask the user if unclear):
- `<version.current>` — version to release = current `pom.xml` version without `-SNAPSHOT`.
- `<version.next>` — next dev version (usually patch incremented), released as `<version.next>-SNAPSHOT`.
- `<release.branch>` / `<hotfix.branch>` — the branch the release is cut from.

Find current/next versions:
```bash
# <version.current>
grep "<version>1" pom.xml | sed -n 's/.*<version>//p' | sed -n 's/-SNAPSHOT<\/version>//p'
# <version.next>
grep "<version>1" pom.xml | sed -n 's/.*<version>//p' | sed -n 's/-SNAPSHOT<\/version>//p' | awk -vFS=- -vOFS=- '{$NF++;print}'
```

On macOS, before any `deploy`/signing step: `export GPG_TTY=$(tty)` so GPG can prompt
for the passphrase.

---

## Standard release — primary (Maven Release plugin)

**Preconditions:** `<release.branch>` was cut from `dev`, is clean and ready; all its
changes are already merged into `dev`; repo cloned over SSH.

```bash
# 1. Switch to the release branch and pull the latest code
git checkout <release.branch>
git pull

# 2. Merge the release branch into main (should be a fast-forward)
git checkout main
git merge <release.branch>

# 3. Prepare the release. Uses the "release" profile / Maven Release plugin.
#    prepare: sets pom to <version.current>, builds, commits+tags the release
#    (tag format is "<version.current>", e.g. 1.0.0 — NOT v1.0.0), then sets pom
#    to <version.next>-SNAPSHOT and commits that.
#    You WILL be prompted for the release version, tag, and next dev version if you
#    don't pass them. -Dgpg.passphrase is optional; omit to be prompted.
mvn -P release clean release:prepare -Darguments=-Dgpg.passphrase="<GPG Passphrase>"

# 4. Do NOT use release:perform. Check out the release tag and deploy — the deploy
#    goal stages and releases to Central. Be patient (can take minutes); track it at
#    https://central.sonatype.com under Deployments.
git checkout <release tag>
mvn -P release clean release:clean deploy -Darguments=-Dgpg.passphrase=<GPG Passphrase>

# 5. Return to main (out of detached HEAD) and push commits + tag together
git checkout main
git push origin main --follow-tags

# 6. Merge the release changes back into dev (should be fast-forward)
git pull
git checkout dev
git merge main
git push origin dev

# 7. Optional: delete the release branch
git push --delete <release.branch>
```

## Standard release — alternative (manual version bump)

Use when you cannot clone over SSH. Same preconditions as the primary flow.

```bash
# 1. Switch to the release branch and pull
git checkout <release.branch>
git pull

# 2. Merge into main (fast-forward)
git checkout main
git merge <release.branch>

# 3. Set the release version across all modules and commit
mvn versions:set -DnewVersion=<version.current>
git add pom.xml utam-core/pom.xml utam-compiler/pom.xml
git commit -m "Releasing <version.current>"

# 4. Tag (format 1.0.21, NOT v1.0.21) and push main + tag
git tag <version.current>
git push origin main
git push origin --tags

# 5. Release the artifact (prompts for GPG passphrase)
mvn -P release clean deploy

# 6. Merge release changes back to dev (fast-forward)
git checkout dev
git merge main

# 7. Bump to the next SNAPSHOT and commit
mvn versions:set -DnewVersion=<version.next>-SNAPSHOT
git add pom.xml utam-core/pom.xml utam-compiler/pom.xml
git commit -m "Preparing for next development iteration <version.next>-SNAPSHOT"
git push origin dev

# 8. Optional: delete the release branch
git push --delete <release.branch>
```

---

## Hotfix release

**Preconditions:** `<hotfix.branch>` was cut from `main`, is clean and ready.
`<merge.branch>` is where the hotfix is merged after release — the `dev` branch, or an
in-flight release branch if one exists (if so, the change reaches `dev` when that
release branch is later merged to `dev`).

```bash
# 1. Switch to the hotfix branch and pull
git checkout <hotfix.branch>
git pull

# 2. Merge into main (fast-forward)
git checkout main
git merge <hotfix.branch>

# 3. Set the release version and commit
mvn versions:set -DnewVersion=<version.next>
git add pom.xml utam-core/pom.xml utam-compiler/pom.xml
git commit -m "Releasing <version.current>"

# 4. Tag and push (currently on main)
git tag <version.next>
git push origin main
git push origin --tags

# 5. Release the artifact (prompts for GPG passphrase)
mvn -P release clean deploy

# 6. Merge into the merge branch — this is almost always a RECURSIVE (--no-ff) merge
git checkout <merge.branch>
git merge --no-ff main
git push origin <merge.branch>

# 7. Optional: delete the hotfix branch
git push --delete <hotfix.branch>
```

---

## Confirmation of release

- There is a TTL delay before the artifact appears on Central — usually within
  **30 minutes**.
- Verify at https://repo1.maven.org/maven2/com/salesforce/utam/ that the new version is
  present for **both** `utam-core` and `utam-compiler`.
- It may take **4+ hours** to appear in https://search.maven.org/.
- **Critical:** Salesforce's internal Nexus uses a 24-hour "not found" cache. If you
  query a version before it exists on Central, that lookup fails and keeps failing for
  24 hours. **Do not** try to build an internal project against the new UTAM version
  until it is confirmed available on Central.

## Downstream steps after a release

### Update UTAM version in core (Appendix B)

Follow: https://git.soma.salesforce.com/pages/dx/docs/core/build/develop-with-the-core-build/add-or-update-a-dependency-version-in-core/

```bash
bazel run //:graph-tool -- set-dependency-version com.salesforce.utam:utam-core --new-version=<version>
```

Note: core pins `utam-core` and `utam-compiler` centrally; bumping the pin updates
every consumer that references the label (see the core build's `third_party/dependencies`).

### Update utam-java-framework (Appendix A)

- Create a PR against the `master` branch of `utam-java-framework`.
- Update the `<utam.framework.version>` property in that repo's `pom.xml` to the new version.
- Merge the PR into `master`.
- `git checkout release && git merge master`, then open a PR against `release` (or push).
- Check the release build:
  https://sfcirelease.sfci.buildndeliver-s.aws-esvc1-useast2.aws.sfdc.cl/sfcirelease/job/LPOP/job/LPOP-utam-java-framework-Jenkinsfile/job/utam-java-framework/job/release/

### Update the JSON schema at Schema-Store

Follow the separate process: "Process for updating and versioning UTAM JSON Schema"
(https://docs.google.com/document/d/1Qplkj1WPMh2EbU8LWpaDm2SBxwWGFRWWNiEJVRZjseE/edit).
