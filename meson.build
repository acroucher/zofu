# Zofu Meson build script

project('zofu', ['fortran'], version: '0.1.0')

if meson.get_compiler('fortran').get_id() == 'gcc'
  f_args = ['-Wno-maybe-uninitialized']
else
  f_args = []
endif

mpi = dependency('mpi', language: 'fortran', required: false)

prefix = get_option('prefix')
libdir = get_option('libdir')

src_dir = join_paths(meson.current_source_dir(), 'src')
zofu_modules = ['zofu.F90', 'zofu_str_utils.F90', 'zofu_scan.F90']
if mpi.found()
  zofu_modules += ['zofu_mpi.F90']
endif
zofu_sources = []
foreach m: zofu_modules
  zofu_sources += [join_paths(src_dir, m)]
endforeach

zofu = shared_library('zofu',
                      zofu_sources,
                      fortran_args: f_args,
                      dependencies: mpi,
                      install: true)

driver_exe = executable(
  'zofu-driver',
  join_paths(src_dir, 'zofu_driver.F90'),
  fortran_args: f_args,
  link_with: zofu,
  install: true,
  install_rpath: join_paths(prefix, libdir))

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

if mpi.found()
  mpi_test_modules = ['test_mpi']
  foreach test_module: mpi_test_modules
    test_exe = executable(
      test_module,
      [join_paths(test_src_dir, test_module + '.F90'),
       join_paths(test_src_dir, 'check_mpi.F90')],
      fortran_args: f_args,
      link_with: zofu,
      dependencies: mpi)
    test(test_module, test_exe, is_parallel: false)
  endforeach
endif

# Zofu scan tests:
  datafiles = ['adder_test.F90', 'adder_setup_test.F90']
  foreach f: datafiles
    configure_file(
      input: join_paths(meson.current_source_dir(), 'test', 'data', f),
      output: f, copy: true)
  endforeach

test_module = 'test_zofu_scan'
test_exe = executable(
  test_module,
  join_paths(test_src_dir, test_module + '.F90'),
  fortran_args: f_args,
  link_with: zofu)
test(test_module, test_exe)
