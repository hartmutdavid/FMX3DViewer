rd /s /q __history
rd /s /q __recovery
rd /s /q Win64

rd /s /q 3dbool\__history
rd /s /q 3dbool\__recovery
rd /s /q Importer\__history
rd /s /q Importer\__recovery
rd /s /q MathFunctions\__history
rd /s /q MathFunctions\__recovery
rd /s /q MeshGen\__history
rd /s /q MeshGen\__recovery

del /q FMX3DViewer.dproj.local
del /q FMX3DViewer.identcache
del /q FMX3DViewer.stat

Pause