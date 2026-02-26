# ===============================
# Assignment 2 – 2048 (Haskell)
# ===============================

# Change this to your MacID
MACID = mannh12
ZIPNAME = A2_$(MACID).zip

# Default target
all: build test

# Build the project
build:
	cabal build

# Run tests
test:
	cabal test

# Run GUI
gui:
	cabal run gui

# Clean build artifacts
clean:
	cabal clean

# Create submission zip (NO dist-newstyle)
zip: clean build test
	rm -f $(ZIPNAME)
	zip -r $(ZIPNAME) . \
		-x "dist-newstyle/*" \
		-x ".git/*" \
		-x ".gitignore" \
		-x "*.zip" \
		-x ".DS_Store" \
		-x "Makefile" \
		-x "cabal.project.freeze"

# Safety check before submission
check:
	cabal build && cabal test

.PHONY: all build test gui clean zip check
