{include resources/header.md}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][mailing-list]
  * [Getting it][downloads]
  * [Documentation][]
  * [News][]
  * [Changelog][]

</div>
<div class="system-description">
    
### What it is

ASDF-System-Connections provides auto-loading of systems that
only make sense when several other systems are loaded. If I'm
lucky, an example will make this clear! [CL-Containers][]
and [CL-Variates][] are separate systems and can therefore
be loaded independently. If both of them are loaded, however,
then it makes sense to also load code that uses CL-Variates
to sample random elements from containers. Rather than
requiring you to remember to load that extra system (and to
load it only after both Cl-Containers and CL-Variates are
loaded), ASDF-System-Connections lets you set things up so
that loading happens automatically.

Here is a simple example from [metabang-bind][]'s system
definition:

    (asdf:defsystem-connection bind-and-metatilities
           :requires (metabang-bind metatilities-base)
           :perform (load-op :after (op c)
                             (use-package (find-package :metabang-bind)
                                          (find-package :metatilities))))

The _requires_ clause specifies the other systems that must
be loaded before this connection will be activated. The rest
of the system definition is regular [ASDF][].
ASDF-System-connections will be loaded as soon as the systems
they require are all loaded and they will only be loaded
once. Before loading a system that uses a system connection,
you should load ASDF-System- Connections in the usual manner:

    (asdf:oos 'asdf:load-op 'asdf-system-connections)


### Mailing Lists

Nope. Sorry, there isn't one. You can, however, contact [Gary
King][gwking-mail]. The best way to keep updated is to follow
the metabang weblog: [unClog][]


### Where is it

A [darcs][] repository is available. The command to get it
is below:

    darcs get http://common-lisp.net/project/cl-containers/asdf-system-connections/darcs/asdf-system-connections

ASDF-System-Connections is [ASDF installable][asdf-install].
Its CLiki home is right [where][cliki-home] you'd expect.

There's also a handy [gzipped tar file][tarball].

{anchor news}

### What is happening

<dl>
<dt>19 October 2008</dt>
<dd>Website rework -- no fire, just smoke
    </dd>
    </dl>
</div>
</div>

{include resources/footer.md}

