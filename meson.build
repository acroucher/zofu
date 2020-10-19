# Zofu Meson build script

project('zofu', ['fortran'], version: '1.0.0')

if meson.get_compiler('fortran').get_id() == 'gcc'
  f_args = ['-Wno-maybe-uninitialized']
else
  f_args = []
endif

if get_option('mpi_wrapper_compiler')
  deps = []
  mpi_found = true
else
  mpi = dependency('mpi', language: 'fortran', required: false)
  deps = mpi
  mpi_found = mpi.found()
endif

prefix = get_option('prefix')
libdir = get_option('libdir')

src_dir = join_paths(meson.current_source_dir(), 'src')
zofu_modules = ['zofu_kinds.F90', 'zofu.F90',
                'zofu_str_utils.F90', 'zofu_scan.F90']
if mpi_found
  zofu_modules += ['zofu_mpi.F90']
endif
zofu_sources = []
foreach m: zofu_modules
  zofu_sources += [join_paths(src_dir, m)]
endforeach

zofu = shared_library('zofu',
                      zofu_sources,
                      fortran_args: f_args,
                      dependencies: deps,
                      install: true)

zofu_dep = declare_dependency(link_with : zofu)

driver_exe = executable(
  'zofu-driver',
  join_paths(src_dir, 'zofu_driver.F90'),
  fortran_args: f_args,
  link_with: zofu,
  install: true,
  install_rpath: join_paths(prefix, libdir))

includedir = get_option('includedir')
if includedir != ''
  module_install_dir = join_paths(prefix, includedir)
  zofu_objs = []
  foreach m: zofu_modules
    zofu_objs += [m + '.o']
  endforeach

  # Failed experiment #1

  # # Construct library build directory name from 'libzofu' + lib_extension + '.p'
  # # or library_file_name + '.p'
  # # Q: How do you get the (full name of/path to) .mod files for a library?
  # # Probably fails at Meson-build time; guessing that zofu.extract_all_objects()
  # # is probably expanded later in ninja. Artifacts not known at Meson-build time
  # libzofu_objs = [zofu.extract_all_objects()]
  # zofu_mods = []
  # foreach m: libzofu_objs
  #   message(m)
  #   # if m.name().endswith('.mod')
  #   #   message('Found mod file ' + m)
  #   #   zofu_mods += [m]
  #   # endif
  # endforeach

  # Failed experiment #2

  # # This probably fails because none of these directories exist at
  # # Meson-build time; it's created later by ninja
  # fs = import('fs')
  # # Start by checking for build directory for .dll
  # mod_path = join_paths('build', 'libzofu.dll.p')
  # if fs.is_dir(mod_path)
  #   # Do nothing: mod_path is set correctly for .dll
  # else
  #   mod_path = join_paths('build', 'libzofu.a.p')
  #   if fs.is_dir(mod_path)
  #     # Do nothing: mod_path is set correctly for .a
  #   else
  #     mod_path = join_paths('build', 'libzofu.so.p')
  #     if fs.is_dir(mod_path)
  #       # Do nothing: mod_path is set correctly for .so
  #     else
  #       # Check location used in older versions of meson
  #       mod_path = join_paths('build', 'zofu@sha')
  #     endif
  #   endif
  # endif
  # message('Checking for .mod files in ' + mod_path)

  # Failed experiment #2.1

  # # This probably fails because zofu.mod doesn't exist at 
  # # Meson-build time; it's created later by ninja
  # zofu_mod_path = join_paths(mod_path, 'zofu.mod')
  # if fs.is_file(zofu_mod_path)
  #   message('Successfully detected ' + zofu_mod_path)
  # endif

  # Brittle but successful experiment #3

  # This might be made more robust if the zofu library build
  # directory name could be predicted at Meson-build time

  # Uncomment one of these:
  # mod_path = join_paths('build', 'libzofu.dll.p')
  # mod_path = join_paths('build', 'libzofu.a.p')
  # mod_path = join_paths('build', 'libzofu.so.p')
  mod_path = join_paths('build', 'zofu@sha')

  # NB this is a temporary measure until Meson gets specific
  # functionality for installing Fortran modules:
  # install_subdir(join_paths('build', 'zofu@sha'),
  install_subdir(mod_path,
                 install_dir: module_install_dir,
                 strip_directory: true,
                 exclude_files: zofu_objs)
  pkg = import('pkgconfig')
  pkg.generate(zofu,
               description: 'Zofu is Object-oriented Fortran Unit-testing')
endif

# Zofu tests:
test_modules = ['test_str_utils',
                'test_logical_asserts',
                'test_integer_asserts',
                'test_real_asserts',
                'test_double_asserts',
                'test_complex_asserts',
                'test_string_asserts']
test_src_dir = join_paths(meson.current_source_dir(), 'test')
foreach test_module: test_modules
  test_exe = executable(
    test_module,
    [join_paths(test_src_dir, test_module + '.F90'),
     join_paths(test_src_dir, 'check.F90')],
    fortran_args: f_args,
    link_with: zofu)
  test(test_module, test_exe)
endforeach

if mpi_found
  mpi_test_modules = ['test_mpi']
  foreach test_module: mpi_test_modules
    test_exe = executable(
      test_module,
      [join_paths(test_src_dir, test_module + '.F90'),
       join_paths(test_src_dir, 'check_mpi.F90')],
      fortran_args: f_args,
      link_with: zofu,
      dependencies: deps)
    test(test_module, test_exe, is_parallel: false)
  endforeach
endif

# Zofu scan tests:
test_module = 'test_zofu_scan'
test_env = environment()
test_env.set('ZOFU_TEST_DATA_PATH',
             join_paths(meson.current_source_dir(), 'test', 'data', ''))
test_exe = executable(
  test_module,
  join_paths(test_src_dir, test_module + '.F90'),
  fortran_args: f_args,
  link_with: zofu)
test(test_module, test_exe, env: test_env)
