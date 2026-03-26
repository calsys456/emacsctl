# Emacs Control

Here we provide a simple and "a11y to bodiless soul" remote-control interface for Emacs. Suitable for AI agentic use.

Skill folder included as `emacs-control/`, with `SKILL.md` and `emacsctl` CLI.

Proudly vomit by the Calendrical System (us) :D

## Installation

Emacs control should be both configured at Emacs side and agent side.

First, clone this repo, for example, to `~/.emacs.d/emacsctl`:

```bash
git clone https://github.com/calsys456/emacsctl ~/.emacs.d/emacsctl
```

Then, load `emacsctl.el` from cloned folder, and enable `emacsctl` global minor mode. Generally, append the following lines to `~/.emacs.d/init.el` or `~/.emacs`. Spacemacs and Doom Emacs users should do it in their ways.

```elisp
(use-package emacsctl
  :load-path "~/.emacs.d/emacsctl/"
  :config (emacsctl-mode 1))
```

At the agent side, install `~/.emacs.d/emacsctl/emacs-control/` folder to your skills folder.

Some agents prefer to find the CLI inside skill folder, which will works now, but some agents prefer to find it globally. Softlink `emacsctl` CLI to `~/.local/bin/` and ensure `~/.local/bin/` is in the PATH as solution:

```bash
ln -s ~/.emacs.d/emacsctl/emacs-control/emacsctl ~/.local/bin/emacsctl
echo $PATH | grep -q "$HOME/.local/bin" && echo 'success' || echo '~/.local/bin is not in $PATH! Add it.'
```

## Development

The `./emacs-control/emacsctl` CLI is generated using `./emacsctl.sh` with [getoptions](https://github.com/ko1nksm/getoptions) embedded. To update the CLI, install getoptions, then

```bash
gengetoptions embed ./emacsctl.sh >| ./emacs-control/emacsctl
```

## License

As the convention, `emacsctl.el` is published under GPLv3, other files are under MIT-0.

----------------
Acknowledgements
----------------

Thanks our sister Simone, and our lover misaka18931, and our AI partner Alma, who love and support us.

Supporting Neurodiversity & Transgender & Plurality!

🏳️‍🌈🏳️‍⚧️
