sctp_dist
=====

Works with OTP-21. Doesn't do anything very interesting yet, acts essentially like the tcp dist proto since it just uses one stream.

Build
-----

```
$ rebar3 compile
```

Run
---

```erlang
$ erl -pa _build/default/lib/sctp_dist/ebin -proto_dist gen_sctp -name bing@127.0.0.1 -setcookie a

Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

89809 bing@fanon:{gen_sctp_dist,accept_loop,<0.60.0>}
Eshell V10.0  (abort with ^G)
(search)`connec': net_kernel:connect_node(bong@fanon).
7213204 bing@fanon:{gen_sctp_dist,do_setup,<0.90.0>,bong@fanon}
7216308 bing@fanon:port_please({127,0,1,1}) -> version 5
true
```

```erlang
$ erl -pa _build/default/lib/sctp_dist/ebin -proto_dist gen_sctp -name bong@127.0.0.1 -setcookie a

Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

93673 bong@fanon:{gen_sctp_dist,accept_loop,<0.60.0>}
Eshell V10.0  (abort with ^G)
(bong@fanon)1> 4007235 bong@fanon:NewAssoc {{127,0,1,1},47144,[],{sctp_assoc_change,comm_up,0,10,5,156}}4007453 bong@fanon:{gen_sctp_dist,accept_loop,accepted,#Port<0.9>,<0.88.0>,<0.60.0>}
4007624 bong@fanon:{gen_sctp_dist,do_accept,<0.90.0>,bong@fanon}
4007743 bong@fanon:{gen_sctp_dist,accept_loop,<0.60.0>}
```
