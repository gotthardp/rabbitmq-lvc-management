PROJECT = rabbitmq_lvc_management

DEPS = rabbit rabbitmq_lvc rabbitmq_management

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

PACKAGES += rabbitmq_lvc
pkg_rabbitmq_lvc_name = rabbitmq_lvc
pkg_rabbitmq_lvc_description = Last value caching exchange
pkg_rabbitmq_lvc_homepage = https://github.com/rabbitmq/rabbitmq-lvc-plugin
pkg_rabbitmq_lvc_fetch = git
pkg_rabbitmq_lvc_repo = https://github.com/rabbitmq/rabbitmq-lvc-plugin.git
pkg_rabbitmq_lvc_commit = master

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

WITH_BROKER_TEST_COMMANDS := \
	eunit:test(rabbit_lvc_mgmt_tests,[verbose])
