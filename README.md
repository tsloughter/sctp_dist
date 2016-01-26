sctp_dist
=====

Requires OTP built with sctp support, `--enable-sctp`, which requires the package `libsctp-dev` on Debian or similar on other distros.

Build
-----

```
$ rebar3 compile
```

Run
---

```erlang
$ erl -pa _build/default/lib/sctp_dist/ebin -proto_dist sctp -sname bing -setcookie a
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
(bing@fanon)1> net_kernel:connect(bong@fanon).
fail
```

```erlang
$ erl -pa _build/default/lib/sctp_dist/ebin -proto_dist sctp -sname bong -setcookie a
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2.1  (abort with ^G)
(bong@fanon)1>
```
