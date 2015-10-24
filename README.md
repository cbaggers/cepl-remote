# Cepl Remote

This is an experiment in controlling cepl from external devices. I want a simple android app to talk to cepl over sockets giving a stream of vec4s (along with basic timing and id data).

This isnt going to be useful to anyone but me for a while but hey, not reason to keep it hidden

Current status:

Can connect the android app to the server and send events. Doesnt contain timing data yet but only a uid for the ui element and the vec4.

Uses the awesome chanl library to threadsafe passing of the messages and Fuse for making the client (it rocks so simple this was)
