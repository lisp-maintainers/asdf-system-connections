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

the current ASDF-System-Connections repository is on
[github][github-asdf-System-Connections] and you can clone it using:

    git clone git://github.com/gwkkwg/asdf-System-Connections

Its CLiki home is right [where][cliki-home] you'd expect.


{anchor news}

### What is happening

<dl>
<dt>24 February 2013</dt>
<dd>Updates to make ASC happ(ier) with ASDF; website tweaks</dd>

<dt>19 October 2008</dt>
<dd>Website rework -- no fire, just smoke</dd>
    </dl>
</div>
</div>

{include resources/footer.md}

