Appache module configuration :

LoadModule wst_module modules/mod_wst.so

<Location /wst>
  SetHandler wst-handler
</Location>