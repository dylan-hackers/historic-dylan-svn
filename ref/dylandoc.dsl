<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!-- Autoswitch magic allows this stylesheet to support both print
     and HTML modes. Note that you might need to edit the CATALOG file
     to define the appropriate pubids for your platform.
-->
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl SYSTEM "nwalsh-modular/html/docbook.dsl" CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl SYSTEM "nwalsh-modular/print/docbook.dsl" CDATA dsssl>
]]>
]>
<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body> 

;; customize the print stylesheet

</style-specification-body>
</style-specification>
<style-specification id="html" use="docbook">
<style-specification-body> 

;; customize the html stylesheet

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>