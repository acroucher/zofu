# Zofu Meson build script

project('zofu', ['fortran'], version: '0.1.0')

zofu_modules = ['zofu.F90']
zofu_sources = []
foreach name: zofu_modules
  zofu_sources += join_paths('src', name)
endforeach

zofu = shared_library('zofu',
                         zofu_sources,
                         install: true)

test_asserts = executable(
  'test asserts', join_paths(meson.current_source_dir(),
                             'test', 'test_asserts.F90'),
  link_with: zofu)
test('test asserts', test_asserts)
