# emacs-laboratory

**Dual-framework Emacs configuration for testing and validation**

> A comprehensive testing ground where Spacemacs and Doom Emacs coexist, sharing unified modules to validate the latest community innovations before distilling them into production-ready starter kits.

[![한국어](https://img.shields.io/badge/한글-README--KO.md-blue)](README-KO.md)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

---

## 🎯 Philosophy

**From Laboratory to Production**

```
emacs-laboratory (experiment) → doomemacs-config (refined)
Add everything → Test thoroughly → Distill to essentials
```

This repository serves as:
- **Testing ground**: Validate features from both Spacemacs and DoomEmacs
- **Knowledge hub**: Integrate latest Emacs community trends
- **Unified platform**: Shared lisp modules for consistent experience
- **AI-ready workspace**: Claude Code integration via CLAUDE.md

---

## 🏗 Architecture

### Dual Framework Support

```
dotemacs/
├──           # Doom Emacs configuration
│   ├── init.el            # Module declarations
│   ├── config.el          # Core configuration
│   └── packages.el        # Package management
├── dotspacemacs/          # Spacemacs configuration
│   ├── init.el            # ~5,100 lines configuration
│   └── spacemacs-keys.el  # Spacemacs-specific keybindings
├── lisp/                  # Shared modules (~29,697 lines)
│   ├── denote-*.el        # Denote system (2,161 lines)
│   ├── org-*.el           # Org-mode (3,599 lines)
│   ├── keys.el            # Unified keybindings (2,193 lines)
│   └── uniconfig.el       # Universal config (1,624 lines)
└── snippets/              # Shared YASnippet templates
```

### Code Statistics

| Component | Lines | Purpose |
|-----------|-------|---------|
| **Spacemacs** | 5,104 | Full Spacemacs configuration |
| **DoomEmacs** | Modular | Doom-style modular config |
| **Shared Lisp** | 29,697 | Framework-agnostic modules |
| **Total** | 37 files | Comprehensive testing environment |

---

## ✨ Core Features

### Knowledge Management
- **Denote**: File-naming system with silo and sequence support
- **Org-roam**: Knowledge graph integration
- **Org-journal**: Daily journaling workflow
- **Org-contacts**: Contact management

### Development
- **Languages**: Python, Nix, JavaScript/TypeScript, Zig, Janet, Elisp
- **Tools**: Magit, LSP/Eglot, Tree-sitter, Direnv, Docker
- **AI/LLM**: GPTel integration, Claude Code support

### Writing & Publishing
- **Org-mode**: Advanced features (babel, capture, agenda)
- **Hugo**: Static site generation
- **Pandoc**: Multi-format export
- **LaTeX**: Academic writing with cdlatex

### UI/UX
- **Evil**: Vim keybindings everywhere
- **Completion**: Corfu + Orderless + Vertico
- **Navigation**: Treemacs, Neotree, window-select
- **Themes**: Custom theme system with Modus themes

---

## 🚀 Quick Start

### Prerequisites

- Emacs 30.2+ (tested on Ubuntu 24.04, NixOS 25.05)
- Git
- Either Spacemacs or Doom Emacs installed

### Installation

**For Doom Emacs:**

```bash
# Clone repository
git clone https://github.com/junghan0611/dotemacs.git ~/dotemacs

# Set DOOMDIR to dotdoomemacs
export DOOMDIR="~/dotemacs/dotdoomemacs"

# Sync Doom
~/.emacs.d/bin/doom sync
```

**For Spacemacs:**

```bash
# Clone repository
git clone https://github.com/junghan0611/dotemacs.git ~/dotemacs

# Symlink or set user-init-file
ln -s ~/dotemacs/dotspacemacs/init.el ~/.spacemacs.d/init.el
```

---

## 📖 Usage

### Framework Switching

**Doom Emacs:**
```bash
DOOMDIR=~/dotemacs/dotdoomemacs emacs
```

**Spacemacs:**
```bash
emacs --load ~/dotemacs/dotspacemacs/init.el
```

### Shared Modules

Both frameworks load from `lisp/`:
- `denote-config.el`, `denote-funcs.el` - Denote system
- `org-config.el`, `org-funcs.el` - Org-mode enhancements
- `keys.el` - Unified keybindings
- `uniconfig.el` - Framework-agnostic settings

---

## 🧪 Testing Workflow

1. **Experiment**: Add new packages/features to either framework
2. **Validate**: Test in daily workflow
3. **Refine**: Move working features to shared lisp modules
4. **Distill**: Extract essentials to [doomemacs-config](https://github.com/junghan0611/doomemacs-config)

### Pipeline

```
emacs-laboratory (full config)
    ↓ validation
    ↓ refinement
doomemacs-config (starter template)
```

---

## 🔗 Related Projects

- **[nixos-config](https://github.com/junghan0611/nixos-config)** - Declarative NixOS environment
- **[doomemacs-config](https://github.com/junghan0611/doomemacs-config)** - Minimal Doom Emacs starter
- **[zotero-config](https://github.com/junghan0611/zotero-config)** - Bibliographic workflow

---

## 📚 Documentation

See project-specific docs:
- **CLAUDE.md** - AI agent configuration
- **README.org** - Original notes (work in progress)

---

## 🛠 Environment

**Tested on:**
- Ubuntu 24.04 LTS 
- NixOS 25.05 
- Emacs 30.2

**Integration:**
- i3wm workflow
- Denote + Org-mode knowledge management
- Digital Garden: [notes.junghanacs.com](https://notes.junghanacs.com)

---

## 🤝 Contributing

This is a personal testing environment, but:
- Issues welcome for questions about configuration patterns
- Fork and adapt for your own dual-framework setup
- Share findings from community trend validation

---

## 📜 License

MIT License

---

**Author**: [@junghan0611](https://github.com/junghan0611)
**Philosophy**: Collaboration over automation
**Created**: 2025-10-11
**Purpose**: 도구의 철학을 세우다 (Establishing the philosophy of tools)
