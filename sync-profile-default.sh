#!/usr/bin/env bash
# Doom Emacs Profile Sync 스크립트
# profiles.el 캐시 재생성 및 패키지 동기화

echo "=== Doom Emacs Default Profile Sync ==="

# 1. 환경 변수 설정
export DOOMPROFILE=default
export DOOMDIR="$HOME/repos/gh/dotemacs/"

echo "Profile: $DOOMPROFILE"
echo "DOOMDIR: $DOOMDIR"

# 2. Doom profiles sync (캐시 생성)
echo ""
echo "Generating profile cache..."
doom profiles sync  

# 3. 프로파일별 동기화
echo ""
echo "Syncing starter profile packages..."
doom sync --profile default -u -j 4

echo ""
echo "=== Sync Complete ==="
echo ""
echo "사용법:"
echo "  1. 직접 실행: ./start-emacs.sh"
echo "  2. 환경변수 설정: export DOOMPROFILE=starter && emacs"
echo "  3. 명령줄 옵션: emacs --profile starter"
