# $testFolder = ("examples")
$testFolder = ("radekw_tests")
$scaleParam = 1

$inputData = Get-ChildItem $testFolder -Filter good*.in | Sort-Object
$expected = Get-ChildItem $testFolder -Filter good*.ps | Sort-Object

For ($I=0; $I -lt $inputData.count; $I++) {
    $in = Join-Path $testFolder ($inputData[$I])
    $out = Join-Path $testFolder (($expected[$I]).Name.Replace("ps", "out"))
    $exp = Join-Path $testFolder ($expected[$I])
    Write-Output $exp
    Write-Output $out
    Get-Content $in | stack run $scaleParam > $out
    Compare-Object (Get-Content $exp) (Get-Content $out) | Write-Output
}
