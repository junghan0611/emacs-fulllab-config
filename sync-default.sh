#!/usr/bin/env bash
# Doom Emacs Default Sync 스크립트 (분리된 설치 버전)

echo "=== Doom Emacs Default Sync ==="

# 환경 변수 설정
export DOOMDIR="$HOME/repos/gh/dotemacs/dotdoomemacs"
DOOM_BIN="$HOME/doomemacs-default/bin/doom"

echo "DOOMDIR: $DOOMDIR"
echo "Doom Bin: $DOOM_BIN"

# 패키지 동기화
echo ""
echo "Syncing default packages..."
$DOOM_BIN sync

echo ""
echo "=== Sync Complete ==="
echo ""
echo "실행 방법:"
echo "  ./start-emacs-default.sh     # GUI 모드로 실행"
echo "  ./start-emacs-default.sh --tui  # 터미널 모드로 실행"