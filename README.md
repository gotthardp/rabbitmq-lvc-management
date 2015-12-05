# RabbitMQ HTTP API for the Last Value Cache

The [Last Value Caching Exchange](https://github.com/rabbitmq/rabbitmq-lvc-plugin.git)
(x-lvc) keeps track of the last value that was published with each routing key,
and when a queue is bound, it automatically enqueues the last value for the binding key.

This plug-in implements an HTTP API for accessing the last published values.

Data published with a routing *key* to an x-lvc *exchange* at *vhost* can
be accessed via HTTP GET:
`http://<server>:15672/api/lvc/`*vhost*`/`*exchange*`/`*key*

The last segment can be omitted. Data published with no routing key to an x-lvc
*exchange* at *vhost* can be thus accessed via HTTP GET:
`http://<server>:15672/api/lvc/`*vhost*`/`*exchange*

Default virtual host is called "/", this will need to be encoded as "%2f".

[![Build Status](https://travis-ci.org/gotthardp/rabbitmq-lvc-management.svg?branch=master)](https://travis-ci.org/gotthardp/rabbitmq-lvc-management)
