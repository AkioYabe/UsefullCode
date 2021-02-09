#sendKeys用
Add-Type -AssemblyName System.Windows.Forms

$input_folder = "C:\Users\akioy\Desktop\input"
$output_folder = "C:\Users\akioy\Desktop\output"
$file_list = Get-ChildItem $input_folder -Filter *.docx | Select-Object Name

$old_printer = Get-WmiObject Win32_Printer | Where-Object default
#プリンタの設定
$printer = Get-WmiObject Win32_Printer | Where-Object Name -eq "Microsoft Print to PDF"
#デフォルトプリンタに設定
$printer.SetDefaultPrinter()

foreach($file in $file_list){
    $input_file = $input_folder+"\"+$file.Name
    $output_file = $output_folder+"\"+[System.IO.Path]::GetFileNameWithoutExtension($file.Name)
    $obj = Start-Process $input_file -Verb Print -PassThru
    #起動待ち
    Start-Sleep -m 1000
    [System.Windows.Forms.SendKeys]::SendWait("$output_file")
    [System.Windows.Forms.SendKeys]::SendWait("{Enter}")
    Wait-Process -InputObject $obj
}

$old_printer.SetDefaultPrinter()
