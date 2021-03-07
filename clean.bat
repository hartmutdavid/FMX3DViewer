rd /s /q __history
rd /s /q __recovery
rd /s /q bin\Win64\Release
del /q bin\Win64\Debug\*.exe
del /q bin\Win64\Debug\*.rsm
rd /s /q dcu

rd /s /q 3dbool\__history
rd /s /q 3dbool\__recovery
rd /s /q Common\__history
rd /s /q Common\__recovery
rd /s /q Importer\__history
rd /s /q Importer\__recovery
rd /s /q Importer\CTMLoader\__history
rd /s /q Importer\CTMLoader\__recovery
rd /s /q Importer\CTMLoader\lzma\__history
rd /s /q Importer\CTMLoader\lzma\__recovery
rd /s /q Importer\FastObjLoader\__history
rd /s /q Importer\FastObjLoader\__recovery
rd /s /q LUXOPHIA\LUX.Brep\__history
rd /s /q LUXOPHIA\LUX.Brep\__recovery
rd /s /q MathFunctions\__history
rd /s /q MathFunctions\__recovery
rd /s /q MeshGen\__history
rd /s /q MeshGen\__recovery

del /q FMX3DViewer.dproj.local
del /q FMX3DViewer.identcache
del /q FMX3DViewer.stat

Pause