# Azubi

is a very simple devops tool, which I'm currently sketching.
Most likely I'll get bored in some days and leave it as it is.

# Goal

Use Haskells awesome type system to create a devop tool like puppet, ansible, salt, chef, ... 

## Design

It should give you a very simple language to describe system states,
but will enforce that systemstate only by some doing bash/ssh calls.
There will be no data of `azubi` left when the call is finished, maybe a log but thats it.
No Cache, no `/etc/azubi/conf`, only the stuff you want there to be.

## Executers

Planed Executers, which are something like renderers of your configuration file.

* BashScript : will create one big bashscript.
* SSHClient : will run some ssh calles to another system.
* Dockerfile : will create a Dockerfile.



