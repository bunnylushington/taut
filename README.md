# 💬 Taut

> **Taut** is a premium, high-fidelity Slack client for GNU Emacs designed for power users who want an immersive, keyboard-driven Slack experience without missing a single thread or reaction.

Built on an asynchronous, thread-safe architecture, Taut features a Gnus-style live Inbox dashboard, a collapsible sidebar, real-time WebSocket syncing (Socket Mode), and a dual-mode interactive thread discussion system.

---

## ✨ Features

- **Multi-Pane Workspace Layout:** Collapsible sidebar (Starred channels, joined Channels, active DMs, watched threads) on the left, main conversation buffer in the center, and interactive threads on the right.
- **Dual-Mode Threads:**
  - **Side-by-Side Right Pane (`RET`):** Opens a dedicated sidebar to discuss threads with full context, complete with active parent-message background highlighting in the main chat.
  - **Inline Collapsible Accordion (`TAB`):** Toggle replies directly inside the main chat stream, beautifully rendered with tree-branch connectors (`│`, `├─`, `└─`).
- **Gnus-Style Unified Inbox:** Instantly tracks and lists unread DMs, direct @mentions, and thread replies.
- **Robust Connection (Zero Deadlocks):** All REST API requests are offloaded to system-native `curl` processes, making Taut completely immune to Emacs's internal GUI/WebSocket network locks.
- **Secure Credentials Storage:** Integration with Emacs's `auth-source` allows loading Slack tokens securely from `~/.authinfo` or `~/.authinfo.gpg`.

---

## 🛠️ Slack Application Setup

To run Taut as yourself (loading your personal channels, DMs, stars, and posting as yourself), you must configure a custom Slack App on the Slack Developer Dashboard.

### Step 1: Create the Slack App
1. Go to the [Slack App Directory](https://api.slack.com/apps) and click **Create New App**.
2. Select **From scratch**, name your app (e.g., `Taut-Emacs`), and choose your workspace.

### Step 2: Configure Scopes (User Token)
To read and post as your personal user, you must set **User Token Scopes** (NOT Bot Token Scopes):
1. In the left sidebar under *Features*, click **OAuth & Permissions**.
2. Scroll down to **Scopes** -> **User Token Scopes**.
3. Add the following scopes:
   - `identify` (verify identity)
   - `channels:read` (list public channels)
   - `channels:history` (fetch public channel messages)
   - `groups:read` (list private channels)
   - `groups:history` (fetch private channel messages)
   - `im:read` (list DMs)
   - `im:history` (fetch DM messages)
   - `mpim:read` (list group DMs)
   - `mpim:history` (fetch group DM messages)
   - `reactions:read` (view message reactions)
   - `reactions:write` (add/remove reactions)
   - `stars:read` (sync starred conversations)
   - `stars:write` (add/remove starred conversations)
   - `users:read` (translate user IDs to usernames)
   - `chat:write` (post messages and replies as yourself)

### Step 3: Enable Socket Mode (App-Level Token)
Socket Mode allows Taut to open a real-time secure WebSocket connection (`wss://`) with Slack to stream activity instantly.
1. In the left sidebar under *Settings*, click **Socket Mode** and toggle **Enable Socket Mode** on.
2. Slack will prompt you to create an **App-Level Token**. Name it (e.g. `taut-socket-mode`) and click **Generate**.
3. Copy this token (it starts with `xapp-...`). This token receives the `connections:write` scope automatically.

### Step 4: Subscribe to Bot Events
This allows Slack to route live stream events over the WebSocket connection:
1. In the left sidebar under *Features*, click **Event Subscriptions** and toggle **Enable Events** on.
2. Under **Subscribe to events on behalf of users**, click **Add Private Channel Event** and add:
   - `message.channels`
   - `message.groups`
   - `message.im`
   - `message.mpim`
   - `reaction_added`
   - `reaction_removed`
3. Click **Save Changes** at the bottom of the page.

### Step 5: Install App and Get User Token
1. Scroll back up the left sidebar and click **Install App**.
2. Click **Install to Workspace** and authorize.
3. Once completed, copy the **User OAuth Token** (starts with `xoxp-...`).

---

## 🔒 Configuration

Taut integrates with Emacs's native `auth-source` library so you don't have to hardcode plaintext tokens in your `init.el`.

Add the following lines to your `~/.authinfo` or `~/.authinfo.gpg` file:

```text
machine api.slack.com login bot password xoxp-YOUR-USER-OAUTH-TOKEN
machine api.slack.com login app password xapp-YOUR-APP-LEVEL-TOKEN
```

### Emacs Init Configuration
Add Taut to your `load-path` or use `straight.el` / `use-package`:

```elisp
(use-package taut
  :load-path "~/projects/taut"
  :config
  ;; Automatically load credentials on startup
  (taut-api-load-tokens-from-authinfo)

  ;; Configure sidebar default width (optional)
  (setq taut-sidebar-width 60))
```

---

## ⌨️ Keybindings Reference

### Sidebar Buffer (`*Taut Sidebar*`)
| Binding | Description |
|:---|:---|
| `RET` / `Click` | Select and open the channel/DM under cursor |
| `g` | Force refresh sidebar unreads and stars list |
| `q` | Bury the sidebar |

### Chat Buffer (`*Taut - #channel*` / `*Taut - @user*`)
| Binding | Description |
|:---|:---|
| `r` | Write and send a new message in the channel |
| `TAB` | Toggle **Inline Accordion** expansion of thread replies |
| `RET` / `t` | Open **Side-by-Side Thread Panel** on the right |
| `a` | Add an emoji reaction to the message at point |
| `g` | Force reload channel history from API |
| `q` | Bury conversation buffer |

### Thread Buffer (`*Taut Thread*`)
| Binding | Description |
|:---|:---|
| `r` | Send a reply to the active thread |
| `g` | Force refresh thread replies from API |
| `q` | Close and hide the side-by-side thread window |

---

## 💻 Development & Compilation

To byte-compile the package cleanly and check for any warnings:

```bash
emacs -L . -batch -f batch-byte-compile *.el
```

Taut compiles cleanly with zero warnings or errors.

---

## 📄 License
Copyright (C) 2026 Bunny Lushington. Distributed under the MIT License.
