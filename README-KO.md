# emacs-fulllab-config

**Spacemacs와 Doom Emacs를 동시에 테스트하고 검증하는 완전한 기능의 이맥스 실험실**

[English Documentation](./README.md)

---

## 개요

Spacemacs와 Doom Emacs 두 프레임워크를 모두 검증하는 **테스트 실험실** 역할을 하는 종합 이맥스 설정 저장소입니다. 이것은 미니멀 스타터킷이 아닙니다. 새로운 패키지, 설정, 워크플로우를 검증한 후 프로덕션 환경으로 정제하기 전에 실험하는 완전한 기능의 환경입니다.

### 목표

- **이중 프레임워크 테스트**: Spacemacs와 Doom Emacs 양쪽에서 기능 검증
- **모듈 공유**: 공통 lisp 모듈을 통한 일관된 사용자 경험 유지
- **파이프라인 아키텍처**: 여기서 실험 → 정제 → [doomemacs-config](https://github.com/junghan0611/doomemacs-config)로 추출
- **커뮤니티 트렌드**: 최신 이맥스 커뮤니티 동향을 실시간으로 추적

---

## 왜 이 프로젝트가 존재하는가

**문제점**: Spacemacs와 Doom Emacs를 위한 별도 설정을 유지하면:
- 새 패키지 테스트 시 중복 작업 발생
- 프레임워크 간 일관되지 않은 사용자 경험
- 어떤 기능이 어디서 작동하는지 추적 어려움

**해결책**: 통합 실험실:
- 두 프레임워크가 약 30,000줄의 공통 lisp 모듈 공유
- 새 기능을 두 환경에서 동시에 테스트
- 검증된 설정을 하류의 간단한 스타터킷으로 흘려보냄

**철학**: "덕지덕지 붙여서 완전히 만들고, 철저히 테스트하고, 무자비하게 단순화하라"

---

## 주요 기능

### 이중 프레임워크 지원

**Spacemacs 설정** (5,104줄)
- 레이어 기반 아키텍처
- spacemacs-keys.el을 통한 커스텀 키바인딩
- Emacs 30+ 호환

**Doom Emacs 설정** (모듈화된 파일들)
- 커스텀 autoload를 포함한 모듈 기반
- Doom의 +everywhere evil 바인딩 통합
- Tree-sitter와 LSP 지원

### 공유 모듈 (총 29,697줄)

**핵심 시스템:**
- **Denote**: 2,161줄 (Hugo 내보내기 포함 개인 지식 관리)
- **Org-mode**: 3,599줄 (GTD, 연락처, 저널, 문학적 프로그래밍)
- **키바인딩**: 2,890줄 (Hydra + 커스텀 키)
- **범용 설정**: 1,624줄 (프레임워크 무관 설정)

**추가 기능:**
- AI 통합 (GPTel, CLAUDE.md를 통한 Claude Code)
- Python/Jupyter 노트북
- Consult-omni 검색
- EKG-Denote 지식 그래프
- 커스텀 테마 및 UI 조정

### 언어 지원

Python, Nix, JavaScript/TypeScript, LaTeX, Shell, Emacs Lisp, C/C++, Janet, Zig 등

---

## 구조

```
emacs-fulllab-config/
├──            # Doom Emacs 설정
│   ├── init.el             # 모듈 선언
│   ├── config.el           # 핵심 설정
│   ├── packages.el         # 패키지 관리
│   ├── +gptel.el           # AI/LLM 통합
│   ├── +magit.el           # Git 워크플로우
│   └── autoload/           # 커스텀 함수
│
├── dotspacemacs/           # Spacemacs 설정
│   ├── init.el             # 레이어 선언 (5,104줄)
│   ├── spacemacs-keys.el   # 커스텀 키바인딩
│   └── emacs-custom.el     # 커스터마이제이션 변수
│
├── lisp/                   # 공유 모듈 (29,697줄)
│   ├── denote-*.el         # 지식 관리
│   ├── org-*.el            # Org-mode 확장
│   ├── keys.el             # 범용 키바인딩
│   ├── hydrakeys.el        # Hydra 메뉴
│   └── uniconfig.el        # 프레임워크 무관 설정
│
├── snippets/               # YASnippet 템플릿
├── captures/               # Org-capture 템플릿
├── local/                  # 로컬 머신 오버라이드
├── var/                    # 런타임 데이터
└── CLAUDE.md               # AI 에이전트 설정
```

---

## 설치

### 사전 요구사항

- Emacs 30+ (Ubuntu 24.04, NixOS 25.05에서 테스트됨)
- Git
- Spacemacs 또는 Doom Emacs 설치

### 빠른 시작

```bash
# 저장소 클론
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/emacs-fulllab-config.git ~/repos/gh/

# Doom Emacs의 경우
# DOOMDIR을 ~//dotdoomemacs로 심볼릭 링크 또는 설정
export DOOMDIR="$HOME//dotdoomemacs"
doom sync

# Spacemacs의 경우
# ~//dotspacemacs를 ~/.spacemacs.d로 심볼릭 링크
ln -s ~//dotspacemacs ~/.spacemacs.d
```

### 디렉토리 설정

설정은 `user--dir`이 저장소 루트를 가리키도록 기대합니다:

```elisp
;; 두 프레임워크 모두 init.el에서 이것을 설정
(setq user--dir "~/dotemacs/")
```

---

## 사용법

### 프레임워크 전환

동일한 기능을 두 프레임워크에서 테스트:

```bash
# Doom Emacs에서 테스트
DOOMDIR="$HOME//dotdoomemacs" doom run

# Spacemacs에서 테스트
emacs --with-profile spacemacs
```

### 새 기능 추가

1. **패키지 추가** `packages.el` 또는 Spacemacs 레이어에
2. **설정** 적절한 파일에 (또는 새 `+feature.el` 생성)
3. **테스트** 두 프레임워크에서
4. **공유 코드 추출** 해당되면 `lisp/`로
5. **문서화** 이 README에

### AI 통합

저장소는 AI 에이전트 설정을 위한 `CLAUDE.md`를 포함합니다:

- 코딩 표준 및 응답 형식 정의
- Digital Garden 워크플로우와 통합
- 한국어/영어 이중 언어 개발 지원

---

## doomemacs-config로의 파이프라인

**이 저장소** → 완전한 실험
**doomemacs-config** → 미니멀 스타터킷 (~2,000줄)

**워크플로우:**
1. "kitchen sink" 접근법으로 새 패키지/기능 테스트
2. Spacemacs와 Doom Emacs 양쪽에서 검증
3. 성공적인 패턴을 정제하고 단순화
4. doomemacs-config로 템플릿 수준의 설정으로 추출

**둘 다 유지하는 이유:**
- `emacs-fulllab-config`: 개인 일상 드라이버, 최첨단 테스트
- `doomemacs-config`: 공유 가능, 미니멀, 터미널 최적화 스타터

---

## 철학

### 실험실 마인드셋

이 설정은 **추가적 실험**을 수용합니다:
- 모든 것을 시도 (덕지덕지 붙여서 - layer upon layer)
- 철저히 검증
- 무자비하게 단순화 (줄이고 줄여서 - reduce and reduce)
- 필수 요소만 공유

### 왜 두 프레임워크인가?

Spacemacs와 Doom Emacs는 이맥스 설정의 **다른 철학**을 대표합니다:
- **Spacemacs**: 레이어 기반, 니모닉 키바인딩, 가이드된 발견
- **Doom Emacs**: 모듈 기반, 성능 중심, 최소 기본값

둘 다 유지함으로써:
- 각 커뮤니티의 최신 혁신을 **학습**
- 공유 모듈이 보편적으로 작동하는지 **검증**
- 사용 사례별 최고의 도구 **선택** (서버 vs 노트북)

### 도구 철학

시리즈의 각 config는 목적을 제공합니다:
- `nixos-config`: 재현 가능한 시스템
- `zotero-config`: 지식 캡처
- `doomemacs-config`: 미니멀 편집기 스타터
- `emacs-fulllab-config`: **종합 검증 환경**

---

## 코드 통계

```
프레임워크별:
  Spacemacs:        5,104줄
  Doom Emacs:     ~10,000줄 (모듈화)

공유 모듈:        29,697줄
  Denote:           2,161줄
  Org-mode:         3,599줄
  키바인딩:         2,890줄
  범용:             1,624줄
  기타 모듈:       19,423줄

총 elisp 파일:    37개 파일
```

---

## 관련 프로젝트

- **[doomemacs-config](https://github.com/junghan0611/doomemacs-config)**: 경량 스타터킷 (하류)
- **[nixos-config](https://github.com/junghan0611/nixos-config)**: 선언적 NixOS 시스템 설정
- **[zotero-config](https://github.com/junghan0611/zotero-config)**: AI 쿼리 가능 서지 워크플로우

---

## 문서화

다음에 대해서는 `CLAUDE.md` 참조:
- AI 에이전트 설정
- 코딩 표준
- 응답 형식
- 프로젝트 컨텍스트

---

## 기여

이것은 개인 설정이지만 자유롭게:
- 특정 모듈에 대한 질문은 이슈로
- 자신만의 실험실을 위해 Fork 및 적용
- 이중 프레임워크 워크플로우에 대한 인사이트 공유

---

## 라이선스

MIT License

## 저자

**정한 (junghanacs)**
- [힣's 디지털가든](https://notes.junghanacs.com)
- [@junghan0611](https://github.com/junghan0611)
- 철학: 자동화보다 협업
