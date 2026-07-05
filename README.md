# 💬 Taut

> **Taut** is a premium, high-fidelity Slack client for GNU Emacs designed for power users who want an immersive, keyboard-driven Slack experience without missing a single thread or reaction.

Built on an asynchronous, thread-safe architecture, Taut features a Gnus-style live Slack Activity inbox dashboard, a collapsible sidebar, real-time WebSocket syncing (Socket Mode), and a dual-mode interactive thread discussion system.

---

## ✨ Features

- **Multi-Pane Workspace Layout:** Collapsible sidebar (Starred channels, joined Channels, active DMs, watched threads, hidden channels) on the left, main conversation buffer in the center, and interactive threads on the right.
- **Dual-Mode Threads:**
  - **Side-by-Side Right Pane (`RET`):** Opens a dedicated sidebar to discuss threads with full context, complete with active parent-message background highlighting in the main chat.
  - **Inline Collapsible Accordion (`TAB`):** Toggle replies directly inside the main chat stream, beautifully rendered with tree-branch connectors (`│`, `├─`, `└─`).
- **Gnus-Style Unified Inbox ("Slack Activity"):** Instantly tracks and lists unread DMs, direct @mentions, and thread replies with robust on-the-fly categorization filters.
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
3. Add the following scopes (all 25 scopes shown in your configuration):
   - `bookmarks:read` (List bookmarks)
   - `bookmarks:write` (Create, edit, and remove bookmarks)
   - `calls:read` (View information about ongoing and past calls)
   - `calls:write` (Start and manage calls in a workspace)
   - `channels:read` (View basic information about public channels)
   - `channels:history` (View messages and other content in public channels)
   - `groups:read` (View basic information about private channels)
   - `groups:history` (View messages and other content in private channels)
   - `im:read` (View basic information about direct messages)
   - `im:history` (View messages and other content in direct messages)
   - `im:write` (Start direct messages with people on your behalf)
   - `mpim:read` (View basic information about group direct messages)
   - `mpim:history` (View messages and other content in group direct messages)
   - `mpim:write` (Start group direct messages with people on your behalf)
   - `reactions:read` (View emoji reactions in channels and conversations)
   - `reactions:write` (Add and edit emoji reactions on your behalf)
   - `stars:read` (View your starred messages and files)
   - `stars:write` (Add or remove stars for your user)
   - `users:read` (View people in your workspace)
   - `chat:write` (Send messages on your behalf)
   - `files:read` (View files shared in channels and conversations)
   - `files:write` (Upload, edit, and delete files on your behalf)
   - `pins:read` (View pinned content in channels and conversations)
   - `pins:write` (Add and remove pinned messages and files on your behalf)
   - `search:read.mpim` (Search workspace content in group direct messages)

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
  (setq taut-sidebar-width 30))
```

---

## ⌨️ Keybindings Reference

### Global Keybindings (All Taut Buffers)
*   `j` : Jump to any channel, group, or DM with autocomplete fuzzy completions (`taut-jump`).
*   `?` : Open the dynamic, context-aware Dispatcher control center (`taut-dispatch`).

### Sidebar Buffer (`*Taut Sidebar*`)
| Binding | Description |
|:---|:---|
| `RET` / `Click` | Open the channel, DM, or watched thread under cursor |
| `TAB` | Toggle collapse / expand state of the section header under cursor |
| `h` | Toggle the hidden status of the channel at point (moves to HIDDEN section) |
| `M` | Mark all messages in the channel at point as read |
| `g` | Force redraw and sync sidebar unreads and stars list |
| `q` | Bury the sidebar |

### Slack Activity (Inbox) Buffer (`*Slack Activity*`)
| Binding | Description |
|:---|:---|
| `RET` / `Click` | Switch directly to the corresponding channel, DM, or thread |
| `d` / `e` | Mark message as read / dismiss and archive from inbox |
| `M` | Mark the entire parent channel of this message as read |
| `a` | Show all activity in the feed (Reset filters) |
| `u` | Filter feed to show only unread items |
| `D` | Filter feed to show only Direct Messages (DMs) |
| `m` | Filter feed to show only direct @mentions |
| `t` | Filter feed to show only watched thread updates |
| `g` | Force reload and refresh the Inbox |
| `q` | Bury the Inbox dashboard |

### Conversation Buffer (`*Taut - #channel*` / `*Taut - @user*`)
| Binding | Description |
|:---|:---|
| `r` | Compose and send a new normal reply to the channel |
| `R` | Reply quoting the message under point |
| `e` | Edit the message under point (if sent by you) |
| `d` | Delete the message under point (if sent by you) |
| `a` | Add an emoji reaction to the message at point |
| `b` / `*` | Star/bookmark the message at point |
| `u` | Upload a file to the channel |
| `n` / `p` | Move cursor to the next / previous message |
| `v` | View native code block or attachment at point in its major mode |
| `c` | Copy code block or attachment at point to clipboard |
| `s` | Save file/attachment at point to disk |
| `TAB` | Toggle **Inline Accordion** expansion of thread replies |
| `RET` / `t` | Open **Side-by-Side Thread Panel** on the right |
| `M` | Mark all messages in this conversation as read |
| `g` | Force reload channel history from API |
| `q` | Bury conversation buffer |

### Thread Buffer (`*Taut Thread*`)
| Binding | Description |
|:---|:---|
| `r` | Compose and send a reply to the active thread |
| `R` | Reply quoting the thread message under point |
| `e` | Edit the thread message under point (if sent by you) |
| `b` / `*` | Star/bookmark the thread message at point |
| `u` | Upload a file directly to the thread |
| `n` / `p` | Move cursor to the next / previous thread message |
| `v` / `c` / `s` | View (`v`), Copy (`c`), or Save (`s`) attachments/code in thread |
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
