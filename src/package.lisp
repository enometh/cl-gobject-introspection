(in-package :common-lisp-user)

(defpackage #:gir
  (:use #:common-lisp #:alexandria #:iterate)
  (:shadow #:get-properties #:ensure-gethash)
  (:export
   #:argument
   #:base-info
   #:type-info
   #:callable-info
   #:function-info
   #:callback-info
   #:signal-info
   #:vfunc-info
   #:error-domain-info
   #:value-info
   #:field-info
   #:registered-type-info
   #:enum-info
   #:interface-info
   #:object-info
   #:struct-info
   #:union-info
   #:property-info
   #:constant-info
   #:arg-info

   #:repository-get-dependencies
   #:repository-is-registered
   #:repository-find-by-gtype
   #:repository-require
   #:repository-get-shared-library
   #:repository-new
   #:repository-get-version
   #:repository-prepend-search-path
   #:repository-load-typelib
   #:repository-get-c-prefix
   #:repository-find-by-name
   #:repository-get-infos
   #:repository-get-typelib-path
   #:repository-get-loaded-namespaces
   #:repository-get-search-path
   #:repository-get-default
   #:info-get-name
   #:info-get-type
   #:info-get-namespace
   #:info-is-deprecated
   #:info-get-typelib
   #:info-get-attributes
   #:info-get-container
   #:info-equal
   #:callable-info-get-args
   #:callable-info-may-return-null
   #:callable-info-get-return-type
   #:callable-info-get-caller-owns
   #:arg-info-get-ownership-transfer
   #:arg-info-may-be-null
   #:arg-info-is-optional
   #:arg-info-get-destroy
   #:arg-info-is-caller-allocates
   #:arg-info-get-scope
   #:arg-info-get-direction
   #:arg-info-get-closure
   #:arg-info-is-return-value
   #:arg-info-get-type
   #:type-tag-to-string
   #:type-info-get-interface
   #:type-info-get-array-fixed-size
   #:type-info-get-array-length
   #:type-info-is-zero-terminated
   #:type-info-is-pointer
   #:type-info-get-param-type
   #:type-info-get-tag
   #:type-info-get-array-type
   #:value-info-get-value
   #:field-info-get-size
   #:field-info-get-type
   #:field-info-get-offset
   #:field-info-get-flags
   #:union-info-get-alignment
   #:union-info-is-discriminated
   #:union-info-get-discriminator-type
   #:union-info-get-discriminator-offset
   #:union-info-get-discriminators
   #:union-info-get-methods
   #:union-info-get-size
   #:union-info-find-method
   #:union-info-get-fields
   #:struct-info-get-fields
   #:struct-info-is-gtype-struct
   #:struct-info-find-method
   #:struct-info-get-methods
   #:struct-info-get-size
   #:struct-info-is-foreign
   #:struct-info-get-alignment
   #:registered-type-info-get-type-init
   #:registered-type-info-get-type-name
   #:registered-type-info-get-g-type
   #:enum-info-get-storage-type
   #:enum-info-get-values
   #:enum-info-get-methods
   #:object-info-get-type-name
   #:object-info-get-interfaces
   #:object-info-get-class-struct
   #:object-info-get-type-init
   #:object-info-find-method
   #:object-info-find-method-using-interfaces
   #:object-info-get-abstract
   #:object-info-get-constants
   #:object-info-get-parent
   #:object-info-get-signals
   #:object-info-find-vfunc
   #:object-info-find-vfunc-using-interfaces
   #:object-info-get-methods
   #:object-info-get-vfuncs
   #:object-info-get-properties
   #:object-info-get-fields
   #:interface-info-get-prerequisites
   #:interface-info-find-method
   #:interface-info-get-signals
   #:interface-info-get-iface-struct
   #:interface-info-get-vfuncs
   #:interface-info-find-vfunc
   #:interface-info-get-properties
   #:interface-info-get-methods
   #:interface-info-get-constants
   #:property-info-get-flags
   #:property-info-get-type
   #:signal-info-get-class-closure
   #:signal-info-true-stops-emit
   #:signal-info-get-flags
   #:vfunc-info-get-signal
   #:vfunc-info-get-offset
   #:vfunc-info-get-address
   #:vfunc-info-get-invoker
   #:vfunc-info-get-flags
   #:constant-info-get-value
   #:constant-info-get-type
   #:function-info-get-property
   #:function-info-get-vfunc
   #:function-info-get-flags
   #:function-info-get-symbol
   #:with-typelib
   #:typelib-new
   #:typelib-symbol
   #:with-typelibs
   #:typelib-namespace
   #:typelib-free

   #:build-translator
   #:build-object-ptr

   #:ffi #:require-namespace
   #:nget
   #:invoke
   #:field
   #:property
   #:allocate-struct
   #:free-struct
   #:with-struct
   #:connect
   #:disconnect

   #:nget-desc
   #:nlist-desc
   #:vairable-desc
   #:name-of
   #:type-desc-of
   #:callable-desc
   #:get-callable-desc
   #:arguments-desc-of
   #:returns-desc-of
   #:object-class
   #:gir-class-of
   #:parent-of
   #:list-fields-desc
   #:get-field-desc
   #:list-properties-desc
   #:get-property-desc
   #:list-methods-desc
   #:get-method-desc
   #:list-class-functions-desc
   #:list-constructors-desc
   #:get-constructor-desc
   #:list-signals-desc
   #:get-signal-desc
   #:list-vfuncs-desc
   #:get-vfunc-desc
   #:interface-desc
   #:list-interfaces-desc
   #:struct-class
   #:enum-desc
   #:values-of

   #:make-trampoline
   #:trampoline-get-function
   #:destroy-trampoline))
