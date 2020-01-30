###############################################################################
# Linters (not normally run manually via command line, but here for backup.)
###############################################################################
.PHONY: lint lint-style lint-style-fix
lint: lint-style
lint-style: lint-style-scala ## Lint all files for code style
lint-style-fix: lint-style-scala-fix # Automatically fix all files for code style

SCALAFMT_IMAGE = mrothy/scalafmt-native:2.3.2
SCALAFMT_TARGETS = shared project build.sbt
SCALAFMT_EXCLUSIONS = target/

lint-style-scala: # Lint scala files for code style
	docker run \
		-v $$(PWD):/src \
		--rm -it \
		--workdir /src \
	$(SCALAFMT_IMAGE)  \
		--exclude $(SCALAFMT_EXCLUSIONS) \
		--list \
		$(SCALAFMT_TARGETS)

lint-style-scala-fix: # Automatically fix scala files for code style
	docker run \
		-v $$(PWD):/src \
		--rm -it \
		--workdir /src \
	$(SCALAFMT_IMAGE)  \
		--exclude $(SCALAFMT_EXCLUSIONS) \
		$(SCALAFMT_TARGETS)

###############################################################################
# Modified version of self-documenting help script from:
# https://suva.sh/posts/well-documented-makefiles/
#
# This creates a default `help` goal that will document all tasks that are
# annotated with a double comment "#" marker.
###############################################################################
.DEFAULT_GOAL:=help
help:  ## Display this help message
	@awk 'BEGIN { \
		FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n" \
	} /^[a-zA-Z0-9_-]+:.*?##/ { \
		printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2 \
	}' $(MAKEFILE_LIST)
