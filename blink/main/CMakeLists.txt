idf_component_register(SRCS "blink_example_main.c"
                       INCLUDE_DIRS ".")

if(CMAKE_C_COMPILER_ID STREQUAL "Clang")
  target_compile_options(${COMPONENT_LIB} PRIVATE
    -Weverything
    -Wno-error=gnu-include-next
    -Wno-error=reserved-identifier
    -Wno-error=reserved-macro-identifier
    -Wno-error=undef
    -Wno-error=language-extension-token
    -Wno-error=gnu-statement-expression-from-macro-expansion
    -Wno-error=pre-c23-compat
    -Wno-declaration-after-statement
    -Wno-missing-prototypes
  )
endif()

                       
set(HC_EXE         "${CMAKE_SOURCE_DIR}/../.build/debug/hc.exe")
set(HYLO_SRC       "${CMAKE_SOURCE_DIR}/main/code.hylo")
set(HYLO_OUT_DIR   "${CMAKE_CURRENT_BINARY_DIR}/hylo-build")
set(HYLO_OBJ       "${HYLO_OUT_DIR}/code.o")          # adjust if hc names it differently
set(HYLO_STDLIB_O  "${HYLO_OUT_DIR}/Hylo.o")
 
add_custom_command(
  OUTPUT "${HYLO_OBJ}"
  BYPRODUCTS "${HYLO_STDLIB_O}"
  COMMAND "${CMAKE_COMMAND}" -E make_directory "${HYLO_OUT_DIR}"
  COMMAND "${HC_EXE}" "${HYLO_SRC}" --emit object-files --freestanding -o "${HYLO_OUT_DIR}"
  DEPENDS "${HYLO_SRC}"
  VERBATIM
  COMMENT "Building Hylo object(s)"
)
 
add_custom_target(hylo_build ALL DEPENDS "${HYLO_OBJ}")
add_dependencies(${COMPONENT_LIB} hylo_build)
 
# Link Hylo objects into this component
target_link_libraries(${COMPONENT_LIB} INTERFACE
  "${HYLO_OBJ}"
  "${HYLO_STDLIB_O}"
)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
