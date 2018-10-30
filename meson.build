# Zofu Meson build script

project('zofu', ['fortran'], version: '0.1.0')

if meson.get_compiler('fortran').get_id() == 'gcc'
  f_args = ['-Wno-maybe-uninitialized']
else
  f_args = []
endif
zofu = shared_library('zofu',
                         join_paths('src', 'zofu.F90'),
                         install: true)
                      fortran_args: f_args,

# Tests:
test_modules = ['test_logical_asserts',
                'test_integer_asserts',
                'test_real_asserts',
                'test_double_asserts',
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
