.PHONY: image test coverage

image:
	docker build -t openlaw/core .

test:
	docker run -v $(PWD)/scripts:/scripts --rm -it openlaw/core /scripts/test.sh

coverage:
	docker run -v $(PWD)/scripts:/scripts --rm -it openlaw/core /scripts/coverage_report.sh
