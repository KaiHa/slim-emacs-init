if (Test-Path -Path "${HOME}\.emacs") {
    Write-Output "[W] Found a .emacs file, please be aware that it might interfere with this configuration."
}
$staticArgs =  "--init-directory=${PSScriptRoot}", "--load=${PSScriptRoot}/init.el"
$combinedArgs = if ($args) { $staticArgs + $args } else { $staticArgs }
Start-Process -WorkingDirectory "$PSScriptRoot" -WindowStyle Hidden -FilePath "C:\Users\ahk2hi\bin\emacs-30.1\bin\emacs.exe" -ArgumentList "${combinedArgs}"
