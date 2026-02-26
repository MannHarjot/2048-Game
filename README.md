# 2048 Game (Haskell)

This project is a Haskell implementation of the game **2048** built for an assignment.

It includes:
- A reusable game engine/library (`src/A2/*`)
- A **CLI version** (terminal UI using `vty`)
- A **GUI version** (windowed UI using `Brillo`)
- A simple **bot player** that can play automatically
- Unit tests using `tasty` / `tasty-hunit`

## What The Project Does

The game follows standard 2048 rules on a 4x4 board:
- Move tiles using arrow keys or `W/A/S/D`
- Matching tiles merge when they collide
- New tiles are spawned after valid moves
- The game ends when no moves are possible

The project separates:
- **Game logic** (`Board`, `Game`, `Grid4x4`, `Quadruple`)
- **Bot logic** (`Bot`)
- **UI layers** (`app/cli`, `app/gui`)

## What I Implemented (Assignment Summary)

- Core 2048 board mechanics (sliding, merging, valid move detection)
- Game state management (`Playing`, `GameOver`, `Quitted`)
- Restart and quit actions
- Human input handling for both CLI and GUI
- Bot toggle + bot step support
- A simple deterministic bot strategy ("first valid move")
- Rendering for terminal and graphical interfaces
- Automated tests for core modules (`Quadruple`, `Grid4x4`, `Board`, `Game`, `Bot`)

## Project Structure

- `src/A2/Board.hs` - board representation and 2048 move logic
- `src/A2/Game.hs` - game state + message/update logic
- `src/A2/Bot.hs` - bot strategy
- `src/A2/Grid4x4.hs`, `src/A2/Quadruple.hs` - data structures/utilities
- `app/cli/Main.hs` - terminal version
- `app/gui/Main.hs` - graphical version (Brillo)
- `test/Main.hs` - unit tests
- `Makefile` - shortcuts for build/test/run

## Requirements

- GHC / Cabal (Haskell toolchain)
- Dependencies are managed by Cabal (see `a2.cabal`)

## How To Run Locally

### 1. Build

```bash
cabal build
```

Or with the Makefile:

```bash
make build
```

### 2. Run Tests

```bash
cabal test
```

Or:

```bash
make test
```

### 3. Run the CLI Version (Terminal)

```bash
cabal run cli
```

### 4. Run the GUI Version (Windowed)

```bash
cabal run gui
```

Or:

```bash
make gui
```

## Controls

- `Arrow keys` or `W/A/S/D` - move tiles
- `B` - toggle bot on/off
- `N` - bot step (CLI only, when bot mode is active)
- `R` - restart game
- `Q` - quit

## Notes

- The current code uses a fixed random seed (`mkStdGen 42`) in both CLI and GUI for reproducible behavior while testing.
- The GUI runs at a low FPS so the bot does not move too quickly.
