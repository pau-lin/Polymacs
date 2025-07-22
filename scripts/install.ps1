$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$VenvDir = Join-Path $ScriptDir "..\env" | Resolve-Path -Relative

if (-not (Test-Path $VenvDir)) {
    python -m venv $VenvDir
}

$ActivateScript = Join-Path $VenvDir "Scripts\Activate.ps1"
if (-not (Test-Path $ActivateScript)) {
    Write-Error "Activate script not found at $ActivateScript"
    exit 1
}

. $ActivateScript

pip install --upgrade pip
pip install beautifulsoup4

try {
    $pandocVersion = pandoc --version | Select-Object -First 1
    Write-Output "Pandoc is installed: $pandocVersion"
} catch {
    Write-Error "Pandoc is not installed. Please install it before continuing."
    exit 1
}

Write-Output "Setup complete."