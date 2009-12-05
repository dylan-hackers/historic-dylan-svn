<%dsp:taglib name="od"/>

<od:standard-header>Implementations</od:standard-header>

<dl>
<lh>The Dylan Team maintains two implementations of Dylan: </lh>

<dt><a href="about-dylan#gd">Gwydion Dylan</a></dt>

<dd>Compiles Dylan to C, is the <a href="ports">most portable</a>, and
generates the <a href="contests">fastest code</a>.</dd>

<dt><a href="about-dylan#od">Open Dylan</a></dt>

<dd>Compiles to native code, has an excellent IDE with support for incremental
development, and has great library support.</dd>

</dl>

<!-- this is too long-winded... -->
<p>Each implementation has its advantages.  The Open Dylan compiler is faster
than Gwydion Dylan's compiler, but Gwydion generates faster code.  Open Dylan
has a very nice IDE, but the IDE doesn't run on Linux yet.  Gwydion runs on
more platforms, but Open Dylan has better library support.  We're working to
fix these discrepancies as fast as possible.  In the meantime, both compilers
support the same core language and a nice set of common libraries
(common-dylan), making it fairly easy to use whichever compiler suits your
needs for a given project.</p>

<p>There is also a byte code compiler called Mindy which is still supported but
no further work will be done on it.  All future work will concentrate on the
two compilers.  Although we are working to make code more portable between
compilers there are still some libraries that only work with one or the
other.</p>

<od:standard-footer/>

