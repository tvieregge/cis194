ghcid-test:
	ghcid \
		--command "stack ghci cis194:lib cis194:test:cis194-test --ghci-options=-fobject-code" --test "main"

lint:
	hlint lint .

.PHONY: ghcid-test lint
