PROJECT = bbs
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0


DEPS = lasp erlog_bbs eredis lager emqtt

dep_erlog_bbs = git https://gitlab.com/yan.guiborat1/erlog_bbs.git

export BUILD_WITHOUT_QUIC = 1
dep_emqtt = git https://github.com/emqx/emqtt.git


include erlang.mk
