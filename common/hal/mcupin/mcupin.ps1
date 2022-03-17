
$XI  = @()
$DI  = @()
$DO  = @()
$OO  = @()

foreach($line in Get-Content .\main.h) {
    if($line -match "#define" -and $line -match "_GPIO_Port"){
		$items = $line.split(" ")
        $pin = $items[1].replace("_GPIO_Port", "")
		$spaces = "                                                    ".substring(1, 50-$pin.length)
		$define = "#define MP_" + $pin.substring(3) + $spaces + " MCUPIN(" + $pin +"_GPIO_Port, " + $pin +"_Pin)"					
		if($pin -match "^XI_") {
			$XI += $define
		}
		elseif($pin -match "^DI_") {
			$DI += $define
		}
		elseif($pin -match "^DO_") {
			$DO += $define
		}
		elseif($pin -match "^OO_") {
			$OO += $define
		}
    }
}


echo ""
echo "/* XI: EXTERN INTERRUPT */"
$XI | Sort-Object

echo ""
echo "/* DI: DIGITALE INPUT */"
$DI | Sort-Object

echo ""
echo "/* DO: DIGITALE OUTPUT */"
$DO | Sort-Object

echo ""
echo "/* OO: DIGITALE OPEN DRAIN OUTPUT */"
$OO | Sort-Object
