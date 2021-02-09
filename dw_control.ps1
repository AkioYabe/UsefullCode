# アセンブリのロード
Add-Type -AssemblyName System.Windows.Forms

#csv読み取り
$path = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $path
$people = Import-Csv .\test.csv

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

# フォームの作成
$form = New-Object System.Windows.Forms.Form
$form.Size = "800,500"
$form.Startposition = "CenterScreen"
$form.Text = "DocuWorks提出"

# ラベル1を作成
newLabel $form "10,20" "250,30" "**ファイルを選んでください"
newLabel $form "400,20" "250,30" "**ファイルを選んでください"
function reflesh {
    param (
        [Windows.Forms.CheckedListBox]$CheckedBox,
        [string]$filter
    )

    $CheckedBox.Items.Clear()
    $filelist = Get-ChildItem "D:\powershell" -Name -Filter $filter
    $CheckedBox.Items.AddRange($filelist)
    $CheckedBox.ClearSelected()
}

# チェックボックス1
$CheckedBox1 = New-Object System.Windows.Forms.CheckedListBox
$CheckedBox1.Location = "10,60"
$CheckedBox1.Size = "370,200"
reflesh $CheckedBox1 "*.ps1"
$form.Controls.Add($CheckedBox1)

# 更新ボタン1
$Reflesh1 = newButton $form "300,20" "75,30" "更新"
$Reflesh1.add_click( { reflesh $CheckedBox1 "*.ps1" })

# チェックボックス2を作成
$CheckedBox2 = New-Object System.Windows.Forms.CheckedListBox
$CheckedBox2.Location = "400,60"
$CheckedBox2.Size = "370,200"
reflesh $CheckedBox2 "*.ps1"
$form.Controls.Add($CheckedBox2)

# 更新ボタンの設定
$Reflesh2 = newButton $form "690,20" "75,30" "更新"
$Reflesh2.add_click( { reflesh $CheckedBox2 "*.ps1" })

# 資料提出
# ラベルを作成
newLabel $form "100,300" "50,15" "提出先"

# コンボボックスを作成
$Combo = New-Object System.Windows.Forms.Combobox
$Combo.Location = New-Object System.Drawing.Point(100, 320)
$Combo.size = New-Object System.Drawing.Size(200, 30)
$Combo.DropDownStyle = "DropDown"
$Combo.FlatStyle = "standard"
$Combo.Items.AddRange((ForEach-Object { $people.name }))
$form.Controls.Add($Combo)

function submit {
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
    Start-Process $iter.address     
}

# 提出ボタン
$Submit = newButton $form "100,350" "75,30" "submit" 
$Submit.add_click({submit})
$Submit.DialogResult = [System.Windows.Forms.DialogResult]::OK

# Docuworks文書作成
newLabel $form "450,300" "100,15" "DWファイル作成"
function create_docuworks {
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
    Start-Process $iter.address     
}

# 作成ボタン
$DW = newButton $form "450,320" "75,30" "create" 
$DW.add_click({create_docuworks})

# PDF作成
newLabel $form "620,300" "100,15" "PDFファイル作成"
function create_pdf {
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
    Start-Process $iter.address     
}

# 作成ボタン
$PDF = newButton $form "620,320" "75,30" "create" 
$PDF.add_click({create_docuworks})

# 最前面に表示：する
# $form.Topmost = $True

# フォームを表示
$result = $Form.ShowDialog()

if ($result -eq "submit") {
    $files = @($CheckedBox1.CheckedItems)
    $iter = $people | Where-Object { $_.name -eq ($Combo.Text) }
}
#エクスプローラーで開く
# Start-Process $iter.address
