#!/usr/bin/env Rscript
# Syncs DESCRIPTION's Version and NEWS.md's heading structure to the target
# version recorded in VERSION, based on comparing it against the latest
# released git tag (vX.Y.Z). Run from the repo root.
#
# - VERSION >  latest tag: promote NEWS.md's "(development version)" section
#   to the target version and bump DESCRIPTION.       (action = "new")
# - VERSION == latest tag: fold any dev-section changes into the existing
#   target section; DESCRIPTION should already match.  (action = "refresh")
# - VERSION <  latest tag, or malformed: hard error.
#
# Writes NEWS.md / DESCRIPTION in place (unless DRY_RUN=true) and writes the
# target version's changelog body to changelog.md. Emits action/version/tag/
# tarball/changed to $GITHUB_OUTPUT when running under GitHub Actions.

dry_run <- identical(Sys.getenv("DRY_RUN"), "true")

die <- function(...) stop(sprintf(...), call. = FALSE)

pkg <- unname(read.dcf("DESCRIPTION", fields = "Package")[1, 1])
if (!file.exists("VERSION")) die("VERSION file not found at repo root.")
target <- trimws(readLines("VERSION")[1])
target_v <- package_version(target) # errors on malformed input

tags <- suppressWarnings(system2("git", c("tag", "-l"), stdout = TRUE))
tags <- grep("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags, value = TRUE)
released_v <- if (length(tags)) max(package_version(sub("^v", "", tags))) else package_version("0.0.0")

if (target_v < released_v) {
  die(
    "VERSION (%s) is older than the latest release (%s) - refusing to proceed.",
    target_v, released_v
  )
}
action <- if (target_v > released_v) "new" else "refresh"

dev_heading <- sprintf("# %s (development version)", pkg)
target_heading <- sprintf("# %s %s", pkg, target)
title_heading <- sprintf("# %s", pkg) # leading "# pkgname" title line, not a version section

version_headings <- function(lines) {
  idx <- grep("^# ", lines)
  idx[trimws(lines[idx]) != title_heading]
}

news_orig <- readLines("NEWS.md")
news <- news_orig
heading_idx <- version_headings(news)
if (!length(heading_idx)) die("NEWS.md has no version headings (only the '%s' title line).", title_heading)

section_body <- function(lines, idx, idx_all) {
  later <- idx_all[idx_all > idx]
  end <- if (length(later)) later[1] - 1L else length(lines)
  if (idx + 1L > end) character(0) else lines[(idx + 1L):end]
}

trim_blank <- function(x) {
  nz <- which(nzchar(trimws(x)))
  if (!length(nz)) return(character(0))
  x[nz[1]:nz[length(nz)]]
}

top_is_dev <- identical(trimws(news[heading_idx[1]]), dev_heading)

if (action == "new") {
  if (!top_is_dev) {
    die(
      "Expected NEWS.md's first heading to be '%s' to promote to %s, found '%s'.",
      dev_heading, target, trimws(news[heading_idx[1]])
    )
  }
  if (any(trimws(news[heading_idx[-1]]) == target_heading)) {
    die("NEWS.md already has a '%s' section - refusing to create a duplicate.", target_heading)
  }
  news[heading_idx[1]] <- target_heading
  heading_idx <- version_headings(news)
  target_idx <- heading_idx[trimws(news[heading_idx]) == target_heading][1]
  changelog <- trim_blank(section_body(news, target_idx, heading_idx))
} else {
  target_matches <- heading_idx[trimws(news[heading_idx]) == target_heading]
  if (!length(target_matches)) {
    die(
      "VERSION (%s) matches the latest release but NEWS.md has no '%s' section to refresh.",
      target, target_heading
    )
  }
  target_idx <- target_matches[1]

  if (top_is_dev) {
    if (length(heading_idx) < 2 || heading_idx[2] != target_idx) {
      die(
        "Expected NEWS.md's '%s' heading to be immediately followed by '%s' for a same-version refresh.",
        dev_heading, target_heading
      )
    }
    dev_idx <- heading_idx[1]
    dev_body <- trim_blank(section_body(news, dev_idx, heading_idx))
    before <- if (dev_idx > 1) news[seq_len(dev_idx - 1L)] else character(0)
    rest <- news[target_idx:length(news)] # starts at the target heading line, unchanged
    if (length(dev_body)) {
      news <- c(before, rest[1], "", dev_body, rest[-1])
    } else {
      news <- c(before, rest)
    }
    heading_idx <- version_headings(news)
    target_idx <- heading_idx[trimws(news[heading_idx]) == target_heading][1]
  }
  changelog <- trim_blank(section_body(news, target_idx, heading_idx))
}

if (!length(changelog)) die("Resolved '%s' section in NEWS.md is empty - nothing to release.", target_heading)

desc_orig <- readLines("DESCRIPTION")
desc_lines <- desc_orig
version_idx <- grep("^Version:", desc_lines)
if (length(version_idx) != 1) die("Expected exactly one 'Version:' line in DESCRIPTION.")
desc_lines[version_idx] <- sprintf("Version: %s", target)

changed <- !identical(news, news_orig) || !identical(desc_lines, desc_orig)

tag <- sprintf("v%s", target)
tarball <- sprintf("%s_%s.tar.gz", pkg, target)

cat(sprintf(
  "action=%s version=%s tag=%s changed=%s\n",
  action, target, tag, changed
))
cat("--- changelog.md ---\n")
cat(paste(changelog, collapse = "\n"), "\n")

if (dry_run) {
  cat("DRY_RUN: not writing NEWS.md / DESCRIPTION / changelog.md\n")
} else {
  if (changed) {
    writeLines(news, "NEWS.md")
    writeLines(desc_lines, "DESCRIPTION")
  }
  writeLines(changelog, "changelog.md")
}

gh_output <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_output)) {
  cat(
    sprintf(
      "action=%s\nversion=%s\ntag=%s\ntarball=%s\nchanged=%s\n",
      action, target, tag, tarball, tolower(as.character(changed))
    ),
    file = gh_output, append = TRUE
  )
}
