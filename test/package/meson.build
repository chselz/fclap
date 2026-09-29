project(
  'fclap-package-consumer',
  'fortran',
  default_options: ['fortran_std=f2018'],
)

fclap = dependency('fclap', version: '>=0.1.0')
consumer = executable(
  'fclap-package-consumer',
  sources: files('main.f90'),
  dependencies: fclap,
)

test('fclap-package-consumer', consumer)
