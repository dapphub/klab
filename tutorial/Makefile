DAPP_DIR=$(CURDIR)
DAPP_SRC=contracts
CONTRACTS_FILE=$(DAPP_SRC)/contracts.sol

SOLC_VERSION=0.5.15
SOLC_FLAGS=

TUTORIAL_FILE=tutorial.act.md

prove: dapp
	nix-shell --pure --command "make prove.shell"

prove.shell:
	klab build
	klab prove-all --dump

dapp:
	nix-shell --pure --command "make dapp.shell"

dapp.shell:
	mkdir -p $(DAPP_DIR)/$(DAPP_SRC)
	echo "pragma solidity 0.5.15;" > $(CONTRACTS_FILE)
	sed -n '/^```solidity/,/^```/ p' < $(TUTORIAL_FILE) | sed '/^```/ d' >> $(CONTRACTS_FILE)
	dapp --version
	cd $(DAPP_DIR) && DAPP_SRC=$(DAPP_SRC) DAPP_SOLC_VERSION=$(SOLC_VERSION) SOLC_FLAGS=$(SOLC_FLAGS) dapp build && cd -

