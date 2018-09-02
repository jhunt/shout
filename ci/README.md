# Release Engineering Notes

The Shout! version number and release code name are stored in
`version.lisp`, which is managed by `ci/version`.

At release time:

    ./ci/version x.y.z Codename

For example, in the first release, we ran

    ./ci/version 0.1.0 Whisper
