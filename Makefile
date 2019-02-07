.PHONY: image test

image:
	docker build -t openlaw/core .

test:
	docker run -v $(PWD)/scripts:/scripts --rm -it openlaw/core /scripts/test.sh
