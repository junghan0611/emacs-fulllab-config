#!/bin/bash
# Doom Emacs Default Profile 실행 스크립트
# GUI 중심 - 데스크톱 환경용
#
# 사용법:
#   ./start-emacs-default.sh          # GUI 모드 (기본)
#   ./start-emacs-default.sh --tui    # 터미널 모드로 실행
#   ./start-emacs-default.sh file.txt # 파일 열기 (GUI 모드)

# emacs-stable 우선 사용
if command -v emacs-stable &> /dev/null; then
    EMACS_BIN="emacs-stable"
else
    EMACS_BIN="emacs"
fi

# Doom 설정 (분리된 설치 사용)
export DOOMDIR="$HOME/repos/gh/dotemacs/"

# TUI 모드 체크 (--tui 옵션이 있으면 터미널로 실행)
if [[ " $@ " =~ " --tui " ]]; then
    echo "Starting Doom Emacs (Terminal mode) with default profile..."
    # --tui 옵션 제거하고 실행
    ARGS="${@//--tui/}"
    MODE="-nw"
else
    echo "Starting Doom Emacs (GUI mode) with default profile..."
    # 기본은 GUI 모드
    MODE=""
    ARGS="$@"
fi

echo "EMACS: $EMACS_BIN"
echo "DOOMDIR: $DOOMDIR"

# Emacs 실행 (--init-directory로 분리된 Doom 사용)
exec $EMACS_BIN --init-directory=$HOME/doomemacs-default $MODE $ARGS
