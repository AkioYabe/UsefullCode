# ロード
Add-Type -AssemblyName System.Windows.Forms

#csv読み取り
Set-Location $psscriptroot
$people = Import-Csv .\member.csv

$DW_path="C:\Users\"+$env:username+"\Documents\Fuji Xerox\DocuWorks\DWFolders\TS"
function newButton {
    param(
        [Windows.Forms.Form]$form ,
        [string]$loc,
        [string]$size,
        [string]$caption
    )

    $btn = New-Object Windows.Forms.Button
    $btn.Location = $loc
    $btn.Size = $size
    $btn.Text = $caption
    $form.Controls.Add($btn)

    return $btn
}

function newLabel {
    param(
        [Windows.Forms.Form]$form ,
        [string]$loc,
        [string]$size,
        [string]$caption
    )

    $label = New-Object System.Windows.Forms.Label
    $label.Location = $loc
    $label.Size = $size
    $label.Text = $caption
    $form.Controls.Add($label)
}

function CreateMail_outlook{
	param(
        [string]$To,
        [string]$CC,
        [string]$Body
    )
    $Outlook=New-Object -ComObject Outlook.Application
    $Mail=$Outlook.CreateItem(0)
    $Mail.To=$To
    $Mail.CC=$CC
    $Mail.Subject="DocuWorksの受信箱にファイルを入れました"
    $Mail.Body=$Body
    $inspector=$Mail.GetInspector
    $inspector.Display()
}

# フォームの作成
$form = New-Object System.Windows.Forms.Form
$form.Size = "800,500"
$form.Startposition = "CenterScreen"
$form.Text = "DocuWorks提出"

# ラベル1を作成
newLabel $form "10,20" "250,30" "DW入れ物(xct)を選んでください"
newLabel $form "400,20" "250,30" "DWファイル(xdw)を選んでください"
function reflesh {
    param (
        [Windows.Forms.CheckedListBox]$CheckedBox,
        [string]$filter
    )

    $CheckedBox.Items.Clear()
    $filelist = Get-ChildItem ($DW_path) -Name -Filter $filter
	if($filelist -ne $null){
		$CheckedBox.Items.AddRange($filelist)
	}
    $CheckedBox.ClearSelected()
}

# チェックボックス1
$CheckedBox1 = New-Object System.Windows.Forms.CheckedListBox
$CheckedBox1.Location = "10,60"
$CheckedBox1.Size = "370,200"
reflesh $CheckedBox1 "*.xct"
$form.Controls.Add($CheckedBox1)

# 更新ボタン1
$Reflesh1 = newButton $form "300,20" "75,30" "更新"
$Reflesh1.add_click( { reflesh $CheckedBox1 "*.xct" })

# チェックボックス2を作成
$CheckedBox2 = New-Object System.Windows.Forms.CheckedListBox
$CheckedBox2.Location = "400,60"
$CheckedBox2.Size = "370,200"
reflesh $CheckedBox2 "*.xdw"
$form.Controls.Add($CheckedBox2)

# 更新ボタンの設定
$Reflesh2 = newButton $form "690,20" "75,30" "更新"
$Reflesh2.add_click( { reflesh $CheckedBox2 "*.xdw" })

# 資料提出
# ラベルを作成
newLabel $form "100,300" "50,20" "提出先"

# コンボボックスを作成
$Combo = New-Object System.Windows.Forms.Combobox
$Combo.Location = New-Object System.Drawing.Point(100, 325)
$Combo.size = New-Object System.Drawing.Size(200, 30)
$Combo.DropDownStyle = "DropDown"
$Combo.FlatStyle = "standard"
$Combo.Items.AddRange((ForEach-Object { $people.name }))
$form.Controls.Add($Combo)

# 提出先フォルダの確認
function open {
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
	Start-Process $iter.folder
}

$Open = newButton $form "100,350" "75,30" "open" 
$Open.add_click({open})

# 提出
function submit {
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
	$file_name=""
	$file_num=0
	foreach($file in $CheckedBox1.CheckedItems){
		Copy-Item ($DW_path+"\"+$file) ($iter.folder+"\"+$file)
        $file_name += $file+"`n"
        $file_num +=1
    }
	$Body = "お疲れ様です。

DocuWorksの受信箱にファイルを入れました。
ご確認の程、よろしくお願いします。
	
$("-"*20)
作業時刻:$((Get-Date).ToString("yyyy/MMM/dd HH:mm"))
ファイル数:$file_num
対象ファイル:
$file_name
"
	CreateMail_outlook $iter.email "takahiro_yamamoto@medicarelife.com" $Body
	Start-Process $iter.folder
}

$Submit = newButton $form "200,350" "75,30" "submit" 
$Submit.add_click({submit})

# Docuworks文書作成
newLabel $form "450,300" "100,20" "DWファイル作成"
function create_docuworks {
    $old_printer = Get-WmiObject Win32_Printer | Where-Object default
    #プリンタの設定
    $printer = Get-WmiObject Win32_Printer | Where-Object Name -eq "DocuWorks Printer"
    #デフォルトプリンタに設定
    $printer.SetDefaultPrinter()

    foreach($file in $CheckedBox2.CheckedItems){
        $input_file = $DW_path+"\"+$file
        $obj = Start-Process $input_file -Verb Print -PassThru
        Wait-Process -InputObject $obj
    }

    $old_printer.SetDefaultPrinter()
}

# 作成ボタン
$DW = newButton $form "450,320" "75,30" "create" 
$DW.add_click({create_docuworks})


# フォームを表示
$result = $Form.ShowDialog()
