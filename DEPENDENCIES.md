# Bundled dependencies

RHO Emacs never installs packages from the network at startup. Third-party
Emacs Lisp is vendored into the repository and put on `load-path` by the
package's `$<name>_append_emacs-init.el`.

## orgwiki: helm and friends

Location: `packages/orgwiki/lisp/helm/`

| Directory   | Source (MELPA)     | Bundled version | Needed by          |
|-------------|--------------------|-----------------|--------------------|
| `helm`      | `helm`             | 20261112.1637   | `org-wiki`         |
| `helm-core` | `helm-core`        | 20261105.1842   | `helm`             |
| `async`     | `async`            | 20261025.1538   | `helm-core`        |
| `wfnames`   | `wfnames`          | 20260706.903    | `helm`             |

`popup` is not required. Dependency chain: `helm` -> `helm-core` + `wfnames`,
`helm-core` -> `async`.

Wiring: `packages/rho/lisp/rho/$orgwiki_append_emacs-init.el` adds the four
directories to `load-path` and runs `(require 'org-wiki)`.

### Update route

Run from the repository root (`D:\dev\rho`) in Git Bash.

1. Find the current versions and check whether the dependency list changed:

   ```sh
   curl -sSfL https://melpa.org/packages/archive-contents -o /tmp/ac.txt
   for p in helm helm-core async wfnames; do grep -m1 "^ ($p \." /tmp/ac.txt; done
   ```

   Each line shows `(NAME . [(YYYYMMDD HHMM) ((DEP (..)) ...) ...`. The version
   is `YYYYMMDD.HHMM` (drop leading zeros of `HHMM`, e.g. `(20260706 903)` is
   `20260706.903`). If the `helm` or `helm-core` dependency list now contains a
   package that is not in the table above, bundle that package too (steps 3-5
   apply to it) and add it to the `dolist` in the init file.

2. Download the tarballs (substitute the versions from step 1; retry on
   `Connection was reset`):

   ```sh
   mkdir -p /tmp/helmdl && cd /tmp/helmdl
   for spec in helm-VERSION helm-core-VERSION async-VERSION wfnames-VERSION; do
     curl -sSfL -o "$spec.tar" "https://melpa.org/packages/$spec.tar"
   done
   ```

3. Replace the bundled copies (delete first so removed upstream files do not
   linger):

   ```sh
   cd /d/dev/rho/packages/orgwiki/lisp/helm
   rm -rf helm helm-core async wfnames
   for spec in /tmp/helmdl/*.tar; do tar -xf "$spec" -C .; done
   mv async-*     async
   mv wfnames-*   wfnames
   mv helm-core-* helm-core
   mv helm-2*     helm        # run after helm-core-* is renamed
   ```

4. Record the new versions: update the table above and the `Helm ...` line in
   `packages/orgwiki/version/orgwiki_version.txt` (keep the first line as the
   package version and bump it, e.g. `1.1.0` -> `1.2.0`).

5. Verify with the bundled Emacs from the repository root:

   ```sh
   software/emacs-x64/bin/emacs.exe -Q --batch --eval '(progn
     (dolist (d (list "packages/orgwiki/lisp/base"
                      "packages/orgwiki/lisp/helm/async"
                      "packages/orgwiki/lisp/helm/wfnames"
                      "packages/orgwiki/lisp/helm/helm-core"
                      "packages/orgwiki/lisp/helm/helm"))
       (add-to-list (quote load-path) (expand-file-name d)))
     (require (quote org-wiki))
     (message "OK helm at %s" (locate-library "helm")))'
   ```

   Expected last line: `OK helm at .../packages/orgwiki/lisp/helm/helm/helm.el`.
   A `define-minor-mode` deprecation warning from `org-wiki.el` is known and
   harmless. Any `Cannot open load file` error means a dependency was added
   upstream: go back to step 1.

6. Review and commit:

   ```sh
   git status packages/orgwiki
   git add packages/orgwiki
   git commit -m "Update bundled helm to VERSION"
   ```

Notes:

- MELPA tarballs contain no `*-autoloads.el`; the init file uses a plain
  `require`, so none is needed.
- Compiled `.elc` files are produced at release time by
  `rho--compile-extensions`; do not commit `.elc` for these directories by hand.
- The upstream projects are https://github.com/emacs-helm/helm,
  https://github.com/jwiegley/emacs-async and
  https://github.com/thierryvolpiatto/wfnames.

## convkit: org-modern and compat

Location: `packages/convkit/lisp/base/`

| Directory    | Source                               | Bundled version | Needed by    |
|--------------|--------------------------------------|-----------------|--------------|
| `org-modern` | MELPA `org-modern`                   | 20261116.739    | ConvKit      |
| `compat`     | GNU ELPA `compat` (not on MELPA)     | 31.1.0.0        | `org-modern` |

`org-modern` replaces `org-bullets`. Its Emacs requirement (29.1) and Org
requirement (9.6) are satisfied by the bundled Emacs. The `compat` here is
independent of the older copy bundled with org-roam
(`packages/orgroam/lisp/org-roam/compat`, 28.1.2.2); the newer one is backward
compatible.

Wiring: `packages/convkit/lisp/rho/$ConvKit_append_emacs-init.el` adds both
directories to `load-path` and enables `global-org-modern-mode` after Org loads.

### Update route

Run from the repository root in Git Bash. Retry any `curl` that fails with
`Connection was reset`.

1. Find the current versions and the dependency list:

   ```sh
   curl -sSf https://melpa.org/packages/archive-contents | grep -m1 "^ (org-modern \."
   curl -sSf https://elpa.gnu.org/packages/archive-contents | grep -A1 "^ (compat$"
   ```

   MELPA shows `(YYYYMMDD HHMM)`, so the tarball version is `YYYYMMDD.HHMM`
   (no leading zeros in `HHMM`). GNU ELPA shows `(31 1 0 0)`, which is
   `31.1.0.0`. If `org-modern` requires a newer `compat` or `org`, bump compat
   first.

2. Download:

   ```sh
   mkdir -p /tmp/omdl && cd /tmp/omdl
   curl -sSfL -o org-modern.tar https://melpa.org/packages/org-modern-VERSION.tar
   curl -sSfL -o compat.tar     https://elpa.gnu.org/packages/compat-VERSION.tar
   ```

3. Replace the bundled copies:

   ```sh
   cd /d/dev/rho/packages/convkit/lisp/base
   rm -rf org-modern compat
   tar -xf /tmp/omdl/org-modern.tar -C . && mv org-modern-* org-modern
   tar -xf /tmp/omdl/compat.tar     -C . && mv compat-*     compat
   rm -rf compat/compat.info compat/dir compat/news compat/NEWS.org
   ```

4. Update the table above.

5. Verify from the repository root:

   ```sh
   software/emacs-x64/bin/emacs.exe -Q --batch --eval '(progn
     (add-to-list (quote load-path) (expand-file-name "packages/convkit/lisp/base/compat"))
     (add-to-list (quote load-path) (expand-file-name "packages/convkit/lisp/base/org-modern"))
     (require (quote org))
     (require (quote org-modern))
     (message "OK org-modern %s" (featurep (quote org-modern))))'
   ```

   Expected last line: `OK org-modern t`.

6. Review and commit (`git status packages/convkit`, then `git add` and commit).
