# SPDX-FileCopyrightText: Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

[confirm: "Prepare and tag release?"]
[doc: "Prepare and tag a release based on semantic commits"]
[no-cd]
release *ARGS='--auto':
  cog bump {{ARGS}}
