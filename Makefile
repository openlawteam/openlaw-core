.PHONY: image test coverage

image:
	docker build -t openlaw/core .

test:
	docker run -v $(PWD)/scripts:/scripts --rm -it openlaw/core /scripts/test.sh

coverage:
	docker run -v $(PWD)/scripts:/scripts --rm -it openlaw/core /scripts/coverage_report.sh

# runs the scalastyle linting, but we are not currently running this in CI, and
# it sounds like our devs aren't running it locally either.
# TODO: we should just consider removing it from plugins/config if not using.
lint:
	docker run \
		-v $(PWD)/scalastyle-config.xml:/src/scalastyle-config.xml \
		--rm -it openlaw/core sbt scalastyle
