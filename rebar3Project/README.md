# Rebar3

**What Rebar3 is:**

http://www.rebar3.org/docs/getting-started

**How to use:**

http://www.rebar3.org/docs/basic-usage

https://ferd.ca/rebar3-shell.html

**This is the tutorial I followed:**

https://medium.com/erlang-central/building-your-first-erlang-app-using-rebar3-25f40b109aad


**How to run this test project (myapp):**

1. cd to myapp dir. (rebar will automatically compile the project in the current path on start)
2. run the command 'rebar3 shell' command (the rebar3 command is defined in this directory).
3. run 'application:start(myapp)' from the rebar3 shell. (this runs myapp_app.erl start/2)

**Adding plugins to project (extra dependencies):**

in rebar.config, add:

    {plugins, [rebar3_hex]}.

then run

    rebar3 hex search cowboy

to find packages to add in rebar.config (does not seem to work in Windows...)

**.app.src purpose:**

used to define app as an OTP application.

**rebar.config purpose:**

defines dependencies to the project
